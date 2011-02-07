
module Camp.Command.Record (record) where

import Camp.Command.Interactive
import Camp.Diff
import qualified Camp.InRepoFileName as InRepoFileName
import Camp.Logging
import Camp.Options
import Camp.Patch.Commute
import Camp.Patch.Equality
import Camp.Patch.MegaPatch
import Camp.Patch.Pretty
import Camp.Patch.Primitive
import Camp.Patch.Sequence
import Camp.Record
import Camp.Repository

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Time
import Unsafe.Coerce

record :: GeneralFlags -> [String] -> IO ()
record gf args = case parseRecordFlags args gf of
                 Right (gf', rf, []) ->
                     record' gf' rf
                 Right (_, _, xs) ->
                     do mapM_ (hPutStrLn stderr . ("Bad arg: " ++ ) . show) xs
                        exitFailure
                 Left errs ->
                     do mapM_ (hPutStrLn stderr) errs
                        exitFailure

record' :: GeneralFlags -> RecordFlags -> IO ()
record' gf rf =
  withLockedRepoSearch $ \r ->
  withLog gf r $ \_ -> do
    n <- genName gf r
    directory <- recordDirectory (inRepoPath r pristineDir) "."
    adds <- recordAdds r
    let primitives = directory `appendSeq` adds
    primitives' <- if rfAll rf
                   then return primitives
                   else do wanted `Then` _ <- interactive pprint primitives
                           return (unsafeCoerce wanted) -- XXX
    -- XXX Check for Nil
    maybeAuthor <- getAuthor r
    author <- case maybeAuthor of
              Just x -> return x
              Nothing ->
                  error "XXX No author found in _camp/repo/prefs/author"
    (short, long) <- case rfMessage rf of
                     -- XXX Should we allow '\n' in rfMessage, and unlines
                     -- it to get the long message?
                     Just msg -> return (BSC.pack msg, BS.empty)
                     Nothing -> getDescription
    time <- getClockTime
    let mi = MetaInfo short long author time
    recordMegaPatch r n mi primitives'

getDescription :: IO (ByteString, ByteString)
getDescription = do putStr "Enter short description: "
                    hFlush stdout
                    line <- getLine
                    -- XXX long is always empty for now
                    return (BSC.pack line, BS.empty)

recordAdds :: LockedRepository -> IO (Seq Primitive from to)
recordAdds r = do fps <- readAdds r
                  writeAdds r []
                  recordAddedFiles fps -- XXX Should pass r through

recordAddedFiles :: forall from to . [FilePath] -> IO (Seq Primitive from to)
recordAddedFiles [] = let resType :: Seq Primitive from to
                          resType = undefined
                      in case startIsEnd resType of
                         IsEqual -> return Nil
recordAddedFiles (fp : fps) = do this <- addedFile fp
                                 rest <- recordAddedFiles fps
                                 return (this `appendSeq` rest)

addedFile :: FilePath -> IO (Seq Primitive from to)
addedFile newPath
    = do new <- BS.readFile newPath
         let fn = InRepoFileName.fromString newPath
             addFile = AddFile fn
         if BS.null new
             then return (addFile `Cons` Nil)
             else let hunk = if (BSC.head new == '\n') ||
                                (BSC.last new == '\n')
                             then Hunk fn 0 0
                                          BS.empty 0
                                          new (BSC.count '\n' new)
                             else Hunk fn 0 0
                                          BS.empty 1
                                          new (BSC.count '\n' new + 1)
                  in return (addFile `Cons` hunk `Cons` Nil)

removedFile :: FilePath -> FilePath -> IO (Seq Primitive from to)
removedFile oldPath newPath
    = do old <- BS.readFile oldPath
         let fn = InRepoFileName.fromString newPath
             rmFile = RmFile fn
         if BS.null old
             then return (rmFile `Cons` Nil)
             else let hunk = if (BSC.head old == '\n') ||
                                (BSC.last old == '\n')
                             then Hunk fn 0 0
                                          old (BSC.count '\n' old)
                                          BS.empty 0
                             else Hunk fn 0 0
                                          old (BSC.count '\n' old + 1)
                                          BS.empty 1
                  in return (rmFile `Cons` hunk `Cons` Nil)

recordDirectory :: FilePath -> FilePath -> IO (Seq Primitive from to)
recordDirectory oldPath newPath = do entries <- getDirectoryContents oldPath
                                     recordEntries entries
    where recordEntries :: forall from to .
                           [FilePath] -> IO (Seq Primitive from to)
          recordEntries [] = let resType :: Seq Primitive from to
                                 resType = undefined
                             in case startIsEnd resType of
                                IsEqual -> return Nil
          recordEntries ("." : es) = recordEntries es
          recordEntries (".." : es) = recordEntries es
          recordEntries (e : es) = do let oldE = oldPath </> e
                                          newE = newPath </> e
                                      stillExists <- doesFileExist newE
                                      this <- if stillExists
                                              then diffFile oldE newE
                                              else removedFile oldE newE
                                      rest <- recordEntries es
                                      return (appendSeq this rest)

