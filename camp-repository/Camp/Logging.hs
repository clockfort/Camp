
-- This is for logging (read: debugging) purposes only

module Camp.Logging (
    Log,
    withLog,
    logRepo,
    ) where

import Camp.Options
import Camp.Repository
import Camp.Utils

import Control.Exception as Exception
import System.Directory
import System.Environment
import System.FilePath

data Log = Log FilePath
         | NoLog

mkLog :: LockedRepository -> Integer -> Log
mkLog r i = Log (inRepoPath r logsDir </> show i)

createLog :: Log -> IO ()
createLog (Log fp) = createDirectory fp
createLog NoLog = return ()

withLogFilePath :: Log -> FilePath -> (FilePath -> IO ()) -> IO ()
withLogFilePath (Log fp) file f = f (fp </> file)
withLogFilePath NoLog _ _ = return ()

withLogDirectoryPath :: Log -> FilePath -> (FilePath -> IO ()) -> IO ()
withLogDirectoryPath (Log fp) dir f = f (fp </> dir)
withLogDirectoryPath NoLog _ _ = return ()

withLog :: GeneralFlags -> LockedRepository -> (Log -> IO a) -> IO a
withLog gf r f
    = if gfLog gf
      then bracket (startLog r) (endLog r) $
                   \l -> f l `Exception.catch` \e -> do logException l e
                                                        throw e
      else f NoLog

startLog :: LockedRepository -> IO Log
startLog r
    = do args <- getArgs
         let nlf = inRepoPath r nextLogFile
         exists <- doesFileExist nlf
         nextLog <- if exists
                    then do xs <- readBinaryFile nlf
                            case maybeRead xs of
                                Just nextLog -> return nextLog
                                Nothing -> panic "Couldn't read nextLog"
                    else return 1
         writeBinaryFile nlf (show (nextLog + 1))
         let l = mkLog r nextLog
         createLog l
         withLogFilePath l "args" $ \fp -> writeBinaryFile fp (show args)
         logRepo l "before" r
         return l

endLog :: LockedRepository -> Log -> IO ()
endLog r l = logRepo l "after" r

logRepo :: Log -> FilePath -> LockedRepository -> IO ()
logRepo l repoName r = withLogDirectoryPath l repoName $ \dp -> copyRepo r dp

#if __GLASGOW_HASKELL__ >= 609
type ExceptionType = SomeException
#else
type ExceptionType = Exception
#endif

logException :: Log -> ExceptionType -> IO ()
logException l e = withLogFilePath l "exception"
                 $ \fp -> writeBinaryFile fp (show e)

