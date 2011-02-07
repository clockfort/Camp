
module Camp.Patch.Primitive (Primitive(..)) where

import Camp.InRepoFileName as InRepoFileName
import Camp.Patch.Apply
import Camp.Patch.Commute
import Camp.Patch.InputOutput
import Camp.Patch.Invert
import Camp.Patch.Pretty
import Camp.Types
import Camp.Utils

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import System.Directory
import System.IO
import Unsafe.Coerce

data Primitive from to where
    AddDir  :: InRepoFileName -> Primitive from to
    RmDir   :: InRepoFileName -> Primitive from to
    MvDir   :: InRepoFileName -> InRepoFileName -> Primitive from to
    AddFile :: InRepoFileName -> Primitive from to
    RmFile  :: InRepoFileName -> Primitive from to
    MvFile  :: InRepoFileName -> InRepoFileName -> Primitive from to
    Hunk    :: InRepoFileName
            -> Bytes      -- Skip this many bytes...
            -> Line       -- ...which means this many lines
            -> ByteString -- Remove these bytes.
            -> Line       -- We removed this many lines
            -> ByteString -- Add these bytes.
            -> Line       -- We added this many lines
            -> Primitive from to
    Binary  :: InRepoFileName
            -> ByteString -- Remove these bytes (the whole file).
            -> ByteString -- Add these bytes (the whole file).
            -> Primitive from to

-- XXX This instance assumes that FilePath is [Char8]
instance InputOutput (Primitive from to) where
    input bs = case BS.head bs of
               0 -> case input (BS.tail bs) of
                    (fn, bs1) -> (AddDir fn, bs1)
               1 -> case input (BS.tail bs) of
                    (fn, bs1) -> (RmDir fn, bs1)
               2 -> case input (BS.tail bs) of
                    (from, bs1) ->
                        case input bs1 of
                        (to, bs2) ->
                            (MvDir from to, bs2)
               3 -> case input (BS.tail bs) of
                    (fn, bs1) -> (AddFile fn, bs1)
               4 -> case input (BS.tail bs) of
                    (fn, bs1) -> (RmFile fn, bs1)
               5 -> case input (BS.tail bs) of
                    (from, bs1) ->
                        case input bs1 of
                        (to, bs2) ->
                            (MvFile from to, bs2)
               6 -> case input (BS.tail bs) of
                    (fn, bs1) ->
                        case input bs1 of
                        (skipBytes, bs2) ->
                            case input bs2 of
                            (skipLines, bs3) ->
                                case input bs3 of
                                (oldBytes, bs4) ->
                                    case input bs4 of
                                    (oldLines, bs5) ->
                                        case input bs5 of
                                        (newBytes, bs6) ->
                                            case input bs6 of
                                            (newLines, bs7) ->
                                                (Hunk fn
                                                      skipBytes
                                                      skipLines
                                                      oldBytes
                                                      oldLines
                                                      newBytes
                                                      newLines,
                                                 bs7)
               7 -> case input (BS.tail bs) of
                    (fn, bs1) ->
                        case input bs1 of
                        (oldBytes, bs2) ->
                            case input bs2 of
                            (newBytes, bs3) ->
                                (Binary fn oldBytes newBytes, bs3)
               _ -> error "InputOutput Primitive: Bad value"
    -- XXX We ought to sanity check the filenames (no leading /, no NUL,
    -- not a reserved name on Windows, etc). Having a Filename type
    -- would help for that; then it would have its own instance.
    -- Not clear we actually want to check for resvered names here, as
    -- we might want a --force for them.
    valid bs _ = case BS.uncons bs of
                 -- AddDir
                 Just (0, bs1) ->
                     valid bs1 (undefined :: InRepoFileName)
                 -- RmDir
                 Just (1, bs1) ->
                     valid bs1 (undefined :: InRepoFileName)
                 -- MvDir
                 Just (2, bs1) ->
                     case valid bs1 (undefined :: InRepoFileName) of
                     Left err -> Left err
                     Right bs2 ->
                         valid bs2 (undefined :: InRepoFileName)
                 -- AddFile
                 Just (3, bs1) ->
                     valid bs1 (undefined :: InRepoFileName)
                 -- RmFile
                 Just (4, bs1) ->
                     valid bs1 (undefined :: InRepoFileName)
                 -- MvFile
                 Just (5, bs1) ->
                     case valid bs1 (undefined :: InRepoFileName) of
                     Left err -> Left err
                     Right bs2 ->
                         valid bs2 (undefined :: InRepoFileName)
                 -- Hunk
                 Just (6, bs') ->
                     valid bs'   (undefined :: InRepoFileName)
                     `thenValid` (undefined :: Bytes)
                     `thenValid` (undefined :: Line)
                     `thenValid` (undefined :: ByteString)
                     `thenValid` (undefined :: Line)
                     `thenValid` (undefined :: ByteString)
                     `thenValid` (undefined :: Line)
                 -- Binary
                 Just (7, bs') ->
                     valid bs'   (undefined :: InRepoFileName)
                     `thenValid` (undefined :: ByteString)
                     `thenValid` (undefined :: ByteString)
                 _ -> Left ("InputOutput Primitive: Bad value", bs)
    output (AddDir fn) = 0 `BS.cons` output fn
    output (RmDir fn) = 1 `BS.cons` output fn
    output (MvDir from to) = 2 `BS.cons`   output from
                               `BS.append` output to
    output (AddFile fn) = 3 `BS.cons` output fn
    output (RmFile fn) = 4 `BS.cons` output fn
    output (MvFile from to) = 5 `BS.cons`   output from
                                `BS.append` output to
    output (Hunk fn skipBytes skipLines oldBytes oldLines newBytes newLines)
        = 6 `BS.cons` output fn
            `BS.append` output skipBytes
            `BS.append` output skipLines
            `BS.append` output oldBytes
            `BS.append` output oldLines
            `BS.append` output newBytes
            `BS.append` output newLines
    output (Binary fn oldBytes newBytes)
        = 7 `BS.cons` output fn
            `BS.append` output oldBytes
            `BS.append` output newBytes

instance Ppr (Primitive from to) where
    ppr (AddDir fn) = text "AddDir" <+> ppr fn
    ppr (RmDir fn) = text "RmDir" <+> ppr fn
    ppr (MvDir from to) = text "MvDir" <+> text (show from)
                                        <+> text (show to)
    ppr (AddFile fn) = text "AddFile" <+> ppr fn
    ppr (RmFile fn) = text "RmFile" <+> ppr fn
    ppr (MvFile from to) = text "MvFile" <+> text (show from)
                                         <+> text (show to)
    ppr (Hunk fn skipBytes skipLines oldBytes oldLines newBytes newLines)
        = text "Hunk" <+> ppr fn
                      <+> text "Skip" <+> integral skipBytes <+> text "bytes,"
                      <+> integral skipLines <+> text "lines"
       $$ nest 4 (
              text "Removal touches" <+>
              integral oldLines <+> text "lines"
           $$ text "Addition touches" <+>
              integral newLines <+> text "lines"
           -- XXX do something about lines?:
           $$ (text $ show $ BSC.unpack oldBytes)
           $$ (text $ show $ BSC.unpack newBytes)
          )
    ppr (Binary {}) = error "XXX Binary"

    pprShow (AddDir fn) = text "AddDir" <+> pprShowAtomic fn
    pprShow (RmDir fn) = text "RmDir" <+> pprShowAtomic fn
    pprShow (MvDir from to) = text "MvDir" <+> pprShowAtomic from
                                           <+> pprShowAtomic to
    pprShow (AddFile fn) = text "AddFile" <+> pprShowAtomic fn
    pprShow (RmFile fn) = text "RmFile" <+> pprShowAtomic fn
    pprShow (MvFile from to) = text "MvFile" <+> pprShowAtomic from
                                             <+> pprShowAtomic to
    pprShow (Hunk fn skipBytes skipLines oldBytes oldLines newBytes newLines)
        = text "Hunk" <+>
              (    pprShowAtomic fn
                $$ (pprShowAtomic skipBytes <+> pprShowAtomic skipLines)
                $$ (pprShowAtomic oldBytes  <+> pprShowAtomic oldLines)
                $$ (pprShowAtomic newBytes  <+> pprShowAtomic newLines)
              )
    pprShow (Binary {}) = error "XXX Binary"

instance Commute Primitive Primitive where
    -- XXX Structuring this needs some thought
    commute (p `Then` q)
        = case (p, q) of
          (Binary {}, _) -> Nothing -- XXX far too conservative
          (_, Binary {}) -> Nothing -- XXX far too conservative
          (AddFile p_f, AddFile q_f) ->
              sanityCheck "AddFile f f" (p_f /= q_f) qp
          (RmFile p_f, RmFile q_f) ->
              sanityCheck "RmFile f f" (p_f /= q_f) qp
          (AddFile p_f, RmFile q_f)
           | p_f == q_f -> Nothing
           | otherwise  -> qp
          (RmFile p_f, AddFile q_f)
           | p_f == q_f -> Nothing
           | otherwise  -> qp
          (AddFile p_f, Hunk q_f _ _ _ _ _ _)
           | p_f == q_f -> Nothing
           | otherwise  -> qp
          (AddDir _, AddDir _) -> Nothing -- XXX far too conservative
          (AddDir _, RmDir _) -> Nothing -- XXX far too conservative
          (AddDir _, AddFile _) -> Nothing -- XXX far too conservative
          (AddDir _, RmFile _) -> Nothing -- XXX far too conservative
          (AddDir _, MvDir _ _) -> Nothing -- XXX far too conservative
          (AddDir _, MvFile _ _) -> Nothing -- XXX far too conservative
          (RmDir _, AddDir _) -> Nothing -- XXX far too conservative
          (RmDir _, RmDir _) -> Nothing -- XXX far too conservative
          (RmDir _, AddFile _) -> Nothing -- XXX far too conservative
          (RmDir _, RmFile _) -> Nothing -- XXX far too conservative
          (RmDir _, MvDir _ _) -> Nothing -- XXX far too conservative
          (RmDir _, MvFile _ _) -> Nothing -- XXX far too conservative
          (AddFile _, AddDir _) -> Nothing -- XXX far too conservative
          (AddFile _, RmDir _) -> Nothing -- XXX far too conservative
          (AddFile _, MvDir _ _) -> Nothing -- XXX far too conservative
          (AddFile _, MvFile _ _) -> Nothing -- XXX far too conservative
          (RmFile _, AddDir _) -> Nothing -- XXX far too conservative
          (RmFile _, RmDir _) -> Nothing -- XXX far too conservative
          (RmFile _, MvDir _ _) -> Nothing -- XXX far too conservative
          (RmFile _, MvFile _ _) -> Nothing -- XXX far too conservative
          (Hunk {}, AddDir _) -> Nothing -- XXX far too conservative
          (Hunk {}, RmDir _) -> Nothing -- XXX far too conservative
          (Hunk {}, MvDir _ _) -> Nothing -- XXX far too conservative
          (Hunk {}, MvFile _ _) -> Nothing -- XXX far too conservative
          (MvDir {}, _) -> Nothing -- XXX far too conservative
          (MvFile {}, _) -> Nothing -- XXX far too conservative
          (AddDir {}, Hunk {})
           -- XXX sanity check hunk file not in dir
           | otherwise  -> qp
          (RmDir {}, Hunk {})
           -- XXX sanity check hunk file not in dir
           | otherwise  -> qp
          (Hunk p_f _ _ _ _ _ _, AddFile q_f) ->
              sanityCheck "AddFile Hunk" (p_f /= q_f) qp
          (RmFile p_f, Hunk q_f _ _ _ _ _ _) ->
              sanityCheck "RmFile Hunk" (p_f /= q_f) qp
          (Hunk p_f _ _ _ _ _ _, RmFile q_f)
           | p_f == q_f -> Nothing
           | otherwise  -> qp
          (Hunk p_fp p_skipBytes p_skipLines
                     p_oldBytes p_oldLines
                     p_newBytes p_newLines,
           Hunk q_fp q_skipBytes q_skipLines
                     q_oldBytes q_oldLines
                     q_newBytes q_newLines)
           | p_fp /= q_fp -> qp
           | p_skipLines + p_newLines < q_skipLines ->
               let byteMovement = BS.length p_newBytes - BS.length p_oldBytes
                   lineMovement = p_newLines - p_oldLines
               in Just (Hunk q_fp (q_skipBytes - byteMovement)
                                  (q_skipLines - lineMovement)
                                  q_oldBytes q_oldLines
                                  q_newBytes q_newLines
                        `Then` p')
           | q_skipLines + q_oldLines < p_skipLines ->
               let byteMovement = BS.length q_newBytes - BS.length q_oldBytes
                   lineMovement = q_newLines - q_oldLines
               in Just (q' `Then`
                        Hunk p_fp (p_skipBytes + byteMovement)
                                  (p_skipLines + lineMovement)
                                  p_oldBytes p_oldLines
                                  p_newBytes p_newLines)
           -- XXX Are any more cases OK?
           | otherwise -> Nothing
        where p' = unsafeCoerce p -- XXX Euch
              q' = unsafeCoerce q -- XXX Euch
              qp = Just (q' `Then` p')

instance Invert Primitive where
    invert (AddDir fn) = RmDir  fn
    invert (RmDir  fn) = AddDir fn
    invert (MvDir  from to) = MvDir to from
    invert (AddFile fn) = RmFile  fn
    invert (RmFile  fn) = AddFile fn
    invert (MvFile  from to) = MvFile to from
    invert (Hunk fn skipBytes skipLines oldBytes oldLines newBytes newLines)
        =   Hunk fn skipBytes skipLines newBytes newLines oldBytes oldLines
    invert (Binary fn oldBytes newBytes) = Binary fn newBytes oldBytes

-- XXX This ignores broken symlinks, has a race condition, etc.
-- But it's portable, and everything is fine as long as you
-- promise to be well-behaved!
instance Apply Primitive where
    apply = applyPrimitive

applyPrimitive :: ApplyState -> Primitive from to -> IO ApplyState
applyPrimitive m prim
    = case prim of
      AddDir fn -> do let fp = InRepoFileName.toFilePath fn
                      fileExists <- doesFileExist fp
                      directoryExists <- doesDirectoryExist fp
                      if fileExists || directoryExists
                          then error ("Already exists: " ++ show fp)
                          else createDirectory fp
                      return m
      RmDir fn -> do let fp = InRepoFileName.toFilePath fn
                     -- XXX Should check for emptiness
                     removeDirectory fp
                     return m
      -- XXX We could be clever here, and just update m if necessary
      MvDir fromFn toFn ->
          do flush m
             let from = InRepoFileName.toFilePath fromFn
                 to   = InRepoFileName.toFilePath toFn
             fromDirectoryExists <- doesDirectoryExist from
             toFileExists <- doesFileExist to
             toDirectoryExists <- doesDirectoryExist to
             if fromDirectoryExists
                 then if toFileExists || toDirectoryExists
                      then error ("Already exists: " ++ show to)
                      else renameDirectory from to
                 else error ("Not a directory: " ++ show from)
             return Nothing
      AddFile fn -> do let fp = InRepoFileName.toFilePath fn
                       fileExists <- doesFileExist fp
                       directoryExists <- doesDirectoryExist fp
                       if fileExists || directoryExists
                           then error ("Already exists: " ++ show fp)
                           else withBinaryFile fp WriteMode $ \_ ->
                                    return ()
                       return m
      RmFile fn ->
          case m of
          Just (fn', _, start, finish)
           | fn == fn' ->
              if BS.null start && BS.null finish
                  then do let fp = InRepoFileName.toFilePath fn
                          -- The file might not exist, as we may be applying
                          --     AddFile f; RmFile f
                          -- But if we get to this point then we know that
                          -- the directory doesn't exist, because either
                          -- AddFile checked it doesn't, or we read the
                          -- file when doing e.g. a Hunk.
                          fileExists <- doesFileExist fp
                          when fileExists $ removeFile fp
                          return Nothing
                  else do flush m
                          error ("Not empty: " ++ show fn)
          _ ->
              do let fp = InRepoFileName.toFilePath fn
                 size <- withBinaryFile fp ReadMode hFileSize
                 if size /= 0
                     then error ("Not empty: " ++ show fn)
                     else removeFile fp
                 return m
      -- XXX We could be clever here, and just update m if necessary
      MvFile fromFn toFn ->
          do flush m
             let from = InRepoFileName.toFilePath fromFn
                 to   = InRepoFileName.toFilePath toFn
             fromFileExists <- doesFileExist from
             toFileExists <- doesFileExist to
             toDirectoryExists <- doesDirectoryExist to
             if fromFileExists
                 then if toFileExists || toDirectoryExists
                      then error ("Already exists: " ++ show to)
                      else renameFile from to
                 else error ("Not a file: " ++ show from)
             return Nothing
      -- XXX Handle m
      Hunk fn skipBytes _ oldBytes _ newBytes _ ->
          case m of
          Just (fn', startBytes, start, finish)
           | fn == fn' ->
          -- XXX This should check that the file is big enough etc
              if skipBytes < startBytes
                  then do let finish' = start `BS.append` finish
                          apply (Just (fn, 0, BS.empty, finish')) prim
                  else case splitAtExactlyBS (skipBytes - startBytes) finish of
                       Just (skipped, finish') ->
                           case splitAtExactlyBS (BS.length oldBytes) finish' of
                               Just (_old, finish'') ->
                                   -- XXX sanity check oldBytes == old
                                   do let start' = start `BS.append`
                                                   skipped `BS.append`
                                                   newBytes
                                          startBytes' = skipBytes
                                                      + BS.length newBytes
                                      return (Just (fn,
                                                    startBytes',
                                                    start',
                                                    finish''))
                               Nothing -> error "Old patch content is wrong"
                       Nothing -> error "Not enough lines to skip"
          _ ->
              do flush m
                 let fp = InRepoFileName.toFilePath fn
                 content <- BS.readFile fp
                 -- XXX Currently we force the length, so that we can
                 -- safely overwrite. Perhaps we should mv to somewhere
                 -- under _camp instead?
                 _ <- evaluate $ BS.length content
                 apply (Just (fn, 0, BS.empty, content)) prim
      -- XXX Handle m
      Binary fn oldBytes newBytes ->
          do flush m
             let fp = InRepoFileName.toFilePath fn
             content <- BS.readFile fp
             if oldBytes == content
                 then BS.writeFile fp newBytes
                 else error "XXX Old content is wrong"
             return Nothing

