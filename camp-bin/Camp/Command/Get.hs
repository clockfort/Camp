
module Camp.Command.Get (get) where

import Camp.Inventory
import Camp.Logging
import Camp.Messages
import Camp.Options
import Camp.Repository

import Control.Exception.Extensible as Exception
import System.Directory
import System.FilePath
import System.IO.Error

get :: GeneralFlags -> [String] -> IO ()
get gf (remoteRepoPath : args)
    = withUnlockedRepo remoteRepoPath $ \remoteRepo -> do
          localDirectory <-
              case args of
              [localDirectory] -> do -- XXX Catch exists exception and complain
                                     createDirectory localDirectory
                                     return localDirectory
              [] -> newDirectory $ takeFileName
                                 $ dropTrailingPathSeparator remoteRepoPath
              _ -> error "XXX Bad arguments to get"
          msg gf Normal ("Creating repository in " ++ show localDirectory)
          setCurrentDirectory localDirectory
          withLockedRepoCreate $ \localRepo ->
              withLog gf localRepo $ \_l -> do
                  -- XXX can't log unlocked repos ATM:
                  -- logRepo l "remote" remoteRepo
                  patches <- getMegaPatches gf remoteRepo
                  initialiseRepo localRepo
                  (filename, offset) <- putMegaPatches localRepo patches
                  remoteInventory <- readInventoryLazily gf remoteRepo
                  let localInventory = compactInventoryWithOffset
                                           filename
                                           offset
                                           remoteInventory
                  writeInventory localRepo localInventory

                  patchesApply <- readMegaPatches gf localRepo
                  applyToPristine localRepo patchesApply

                  -- Rather than applying all the patches again with
                  -- applyToWorking, we just copy the result from pristine.
                  -- This is normally much faster.
                  copyPristineToWorking localRepo
get _ _ = error "XXX Bad arguments to get"

-- XXX Should sanity check the filepath, e.g. not "", "/foo" or "foo/bar"
-- XXX Should we also check for non-printable-ascii chars or something?
newDirectory :: FilePath -> IO FilePath
newDirectory fp = do createDirectory fp
                     return fp
              `whenAlreadyExists` f 0
    where f :: Integer -> IO FilePath
          f i = do let fp' = fp ++ "_" ++ show i
                   createDirectory fp'
                   return fp'
            `whenAlreadyExists`
                f (i + 1)

whenAlreadyExists :: IO a -> IO a -> IO a
whenAlreadyExists f g = f `Exception.catch` \e -> if isAlreadyExistsError e
                                                  then g
                                                  else throw e

