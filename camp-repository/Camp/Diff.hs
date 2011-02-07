
module Camp.Diff (diffFile) where

import Camp.InRepoFileName as InRepoFileName
import Camp.Patch.Equality
import Camp.Patch.Primitive
import Camp.Patch.Sequence
import Camp.Types
import Camp.Utils

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.List
import Data.List.LCS

diffFile :: FilePath -> FilePath -> IO (Seq Primitive from to)
diffFile oldPath newPath
 = do old <- BS.readFile oldPath
      new <- BS.readFile newPath
      let oldLines = myLines old
          newLines = myLines new
          commonLines = lcs oldLines newLines
          fn = InRepoFileName.fromString newPath
      return $ mkDiff fn 0 0 commonLines oldLines newLines

mkDiff :: forall from to .
          InRepoFileName -> Bytes -> Line
       -> [ByteString] -> [ByteString] -> [ByteString]
       -> Seq Primitive from to
-- XXX This is all a bit fiddly
mkDiff fn skipBytes skipLines cs os ns
 = case (cs, os, ns) of
   ([], [], []) -> let resType :: Seq Primitive from to
                       resType = undefined
                   in case startIsEnd resType of
                      IsEqual -> Nil
   (common:cs', old:os', new:ns')
    | common == old && common == new
       -> mkDiff fn (skipBytes + BS.length common + 1{- '\n' -})
                    (skipLines + 1) cs' os' ns'
   (common:_, _, _) ->
       mkHunkThenRest appendNewLines skipBytes (break (common ==) os)
                                               (break (common ==) ns)
   ([], _, _)
    | skipBytes == 0 ->
       mkHunkThenRest intersperseNewLines skipBytes       (os, []) (ns, [])
    | otherwise ->
       -- Here we have to unskip the last '\n' we skipped, as we need
       -- to remove it
       mkHunkThenRest prefixNewLines      (skipBytes - 1) (os, []) (ns, [])
 where appendNewLines = map (`BSC.append` BSC.singleton '\n')
       intersperseNewLines = intersperse (BSC.singleton '\n')
       prefixNewLines = map ('\n' `BSC.cons`)

       mkHunkThenRest _ _ ([], _) ([], _)
           = error "XXX mkDiff: Can't happen"
       mkHunkThenRest addNewLines skipBytes' (reallyOld, os') ([], _) =
           Cons (Hunk fn skipBytes' skipLines
                         (BS.concat (addNewLines reallyOld))
                         (genericLength reallyOld)
                         BS.empty 0)
                (mkDiff fn skipBytes skipLines cs os' ns)
       mkHunkThenRest addNewLines skipBytes' ([], _) (reallyNew, ns') =
           let newBytes = BS.concat (addNewLines reallyNew)
               newLines = genericLength reallyNew
               skipBytes'' = skipBytes' + BS.length newBytes
               skipLines' = skipLines + newLines
           in Cons (Hunk fn skipBytes' skipLines
                         BS.empty 0
                         newBytes newLines)
                   (mkDiff fn skipBytes'' skipLines' cs os ns')
       -- If we have both removals and additions then we don't need
       -- to worry about newlines - we leave newlines before and
       -- after the hunk, if they were there
       mkHunkThenRest _ _ (reallyOld, os') (reallyNew, ns') =
           let oldBytes = BS.intercalate (BSC.singleton '\n') reallyOld
               oldLines = genericLength reallyOld
               newBytes = BS.intercalate (BSC.singleton '\n') reallyNew
               newLines = genericLength reallyNew
               skipBytes' = skipBytes + BS.length newBytes + 1{- '\n' -}
               skipLines' = skipLines + newLines
           in Cons (Hunk fn skipBytes skipLines
                         oldBytes oldLines
                         newBytes newLines)
                   (mkDiff fn skipBytes' skipLines' cs os' ns')

