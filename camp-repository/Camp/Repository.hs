
module Camp.Repository (
    LockedRepository,
    UnlockedRepository,
    Repository,
    withLockedRepo,
    withLockedRepoCreate,
    withLockedRepoSearch,
    withUnlockedRepo,
    withUnlockedRepoSearch,
    initialiseRepo,
    appendInventoryItem,
    readInventory,       -- XXX Shouldn't be exported?
    readInventoryLazily, -- XXX Shouldn't be exported?
    readLocalInventory,
    writeInventory,
    readAdds,
    writeAdds,
    getContent,
    getMegaPatches,
    putMegaPatches,
    -- XXX readMegaPatch,
    readMegaPatches,
    readMegaPatchesWithAbsoluteInventory,
    writeMegaPatch,
    writeMegaPatches,
    pristineDir,
    applyToPristine,
    applyToWorking,
    copyPristineToWorking,
    genName,
    getAuthor,
    copyRepo,
    logsDir,
    nextLogFile,
    inRepoURL,
    inRepoPath,
    ) where

import Camp.Content
import Camp.InRepoFileName as FN
import Camp.Inventory
import Camp.Messages
import Camp.Network as Network
import Camp.Options
import Camp.Patch.Apply
import Camp.Patch.InputOutput
import Camp.Patch.MegaPatch
import Camp.Patch.Name
import Camp.Patch.Sequence
import Camp.Patch.Stream
import Camp.Types
import Camp.Utils

import Control.Exception.Extensible
import Control.Monad
import Data.ByteString.Lazy (ByteString)
-- XXX Euch, can we get defaultChunkSize added to the official interface?
-- Or should re define it locally?
import qualified Data.ByteString.Lazy.Internal as BS (defaultChunkSize)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Char
import Data.IORef
import Numeric
import System.Directory
import System.Exit
import qualified System.FilePath as FilePath
import System.IO hiding (getContents) -- XXX
import System.IO.Error
import System.IO.Unsafe
import System.Time

import Prelude hiding (getContents) -- XXX

data LockedRepository = LockedRepository
         String                  -- The repository location

data UnlockedRepository = UnlockedRepository
         URL                      -- The repository location
         (IORef (Maybe FilePath)) -- The inventory
         (IORef (Maybe FilePath)) -- The patches
         -- XXX Should we also cache the relative inventory?

class Repository r where
    repoURL :: r -> URL
    getInventory :: GeneralFlags -> r -> IO ByteString
    readLocalInventory :: GeneralFlags -> r -> IO AbsoluteInventory
    getMegaPatches :: GeneralFlags -> r -> IO ByteString

instance Repository LockedRepository where
    repoURL (LockedRepository fp) = mkURL ("file://" ++ fp)
    getInventory = getInventoryLocked
    readLocalInventory gf r@(LockedRepository fp)
        = do inv <- readInventory gf r
             return $ toAbsoluteInventory fp inv
    getMegaPatches gf r = do msg gf Verbose "Getting patches from locked repo"
                             inv <- readInventoryLazily gf r
                             getMegaPatchesWithInventory gf r inv

getInventoryLocked :: GeneralFlags -> LockedRepository -> IO ByteString
getInventoryLocked _ r = BS.readFile (inRepoPath r inventoryFile)

instance Repository UnlockedRepository where
    repoURL (UnlockedRepository loc _ _) = loc
    getInventory = getInventoryUnlocked
    readLocalInventory gf r
        = do bs <- getInventory gf r
             fp <- getUnlockedRepositoryPatchesLocalFilePath gf r
             case (fst $ input bs) :: Stream InRepoInventoryItem of
                 Stream is -> return $ compactInventory fp is
    getMegaPatches gf r
        = do msg gf Verbose "Getting patches from unlocked repo"
             fp <- getUnlockedRepositoryPatchesLocalFilePath gf r
             BS.readFile fp

getInventoryUnlocked :: GeneralFlags -> UnlockedRepository -> IO ByteString
getInventoryUnlocked _ r
    = do fp <- getUnlockedRepositoryInventoryLocalFilePath r
         BS.readFile fp

getUnlockedRepositoryInventoryLocalFilePath :: UnlockedRepository
                                            -> IO FilePath
getUnlockedRepositoryInventoryLocalFilePath r@(UnlockedRepository _ ref _)
 = do mfp <- readIORef ref
      case mfp of
          Just fp -> return fp
          Nothing -> do
              let repoInventory = inRepoURL r inventoryFile
              bracketOnError (downloadToTemporaryFile repoInventory)
                             (\fp -> do tryJust (guard . isDoesNotExistError)
                                                (removeFile fp))
                             $ \fp ->
                  do bs <- BS.readFile fp
                     case valid bs (undefined :: Stream InRepoInventoryItem) of
                         Left (err, _) ->
                             error ("XXX Bad remote inventory " ++ err)
                         Right bs' ->
                             unless (BS.null bs') $ error "XXX too many bytes"
                     writeIORef ref (Just fp)
                     return fp

getUnlockedRepositoryPatchesLocalFilePath :: GeneralFlags -> UnlockedRepository
                                          -> IO FilePath
getUnlockedRepositoryPatchesLocalFilePath gf r@(UnlockedRepository _ _ ref)
 = do mfp <- readIORef ref
      case mfp of
          Just fp -> return fp
          Nothing ->
              do inv1 <- readInventoryLazily gf r
                 let cs = inventoryToContentURLs (repoURL r) inv1
                 bracketOnError (downloadContentsToTemporaryFile cs)
                                (\fp -> do _ <- tryJust (guard . isDoesNotExistError)
                                                        (removeFile fp)
                                           writeIORef ref Nothing)
                                $ \fp ->
                     do -- The remote repo may not be correctly formed,
                        -- so we need to check that each element of the
                        -- inventory actually points to a syntactically
                        -- valid patch. Just reading the patches when we
                        -- apply them won't do it, as we read the whole
                        -- contents and then read it as a list of
                        -- patches, so the boundary between patches may
                        -- be in the wrong place.
                        inv2 <- readInventoryLazily gf r
                        bs <- BS.readFile fp
                        checkInventoryMatchesPatches inv2 bs
                        writeIORef ref (Just fp)
                        return fp

checkInventoryMatchesPatches :: InRepoInventory -> ByteString -> IO ()
checkInventoryMatchesPatches inv = f (compactInventory noFile inv)
    where noFile = error "checkInventoryMatchesPatches: Can't happen"
          f [] bs = unless (BS.null bs) $ error "XXX too many bytes"
          f (InventoryItem _ _ _ len : is) bs
              = -- We assume that the total file size is right, or the
                -- download should have failed.
                -- So we can use splitAt rather than splitAtExactlyBS,
                -- and know that we won't get any short splits.
                -- This is important, as early commit patches tend to
                -- be large (e.g. GHC has a 21M patch near the start),
                -- so we don't want to keep a whole patch in memory
                -- while we're checking that we have enough bytes.
                case BS.splitAt len bs of
                (this, rest) ->
                    case valid this (undefined :: MegaPatch from to) of
                    Left (err, _) ->
                        error ("XXX checkInventoryMatchesPatches " ++ err)
                    Right bs' ->
                        do unless (BS.null bs') $ error "XXX too many bytes"
                           f is rest

-- This contains the repo proper, as well as the logs
repoBase :: InRepoFileName
repoBase = fromString "_camp"

repoRoot :: InRepoFileName
repoRoot = repoBase FN.</> fromString "repo"

inventoryFile :: InRepoFileName
inventoryFile = repoRoot FN.</> fromString "inventory"

addsFile :: InRepoFileName
addsFile = repoRoot FN.</> fromString "adds"

prefsDir :: InRepoFileName
prefsDir = repoRoot FN.</> fromString "prefs"

authorFile :: InRepoFileName
authorFile = prefsDir FN.</> fromString "author"

pristineDir :: InRepoFileName
pristineDir = repoRoot FN.</> fromString "pristine"

workingDir :: InRepoFileName
workingDir = fromString ""

-- XXX This ought to be atomic given the current repo format, but it isn't
appendInventoryItem :: LockedRepository -> InRepoInventoryItem -> IO ()
appendInventoryItem r i
    = do h <- openBinaryFile (inRepoPath r inventoryFile) AppendMode
         BS.hPutStr h (output i)
         hClose h

-- XXX write this differently?
readInventory :: Repository r => GeneralFlags -> r -> IO InRepoInventory
readInventory gf r = do inv <- readInventoryLazily gf r
                        _ <- evaluate $ length inv
                        return inv

readInventoryLazily :: Repository r => GeneralFlags -> r -> IO InRepoInventory
readInventoryLazily gf r
    = do content <- getInventory gf r
         -- XXX check snd == ""?
         case fst $ input content of
             Stream is -> return is

-- XXX Currently we assume this is atomic...which probably isn't true
writeInventory :: LockedRepository -> InRepoInventory -> IO ()
writeInventory r ns = BS.writeFile (inRepoPath r inventoryFile)
                                   (output (Stream ns))

readAdds :: LockedRepository -> IO [FilePath]
readAdds r = do content <- readBinaryFile (inRepoPath r addsFile)
                case maybeRead content of
                    Just fps -> return fps
                    Nothing -> panic ("Corrupt adds?\n" ++ content)

writeAdds :: LockedRepository -> [FilePath] -> IO ()
writeAdds r ns = writeBinaryFile (inRepoPath r addsFile) (show ns)

-- XXX This should be composed from other InRepoFileName's
patchesDir :: InRepoFileName
patchesDir = repoRoot FN.</> fromString "patches"

patchFile :: InRepoFileName
patchFile = patchesDir FN.</> fromString "patchFile"

inRepoPath :: LockedRepository -> InRepoFileName -> FilePath
inRepoPath (LockedRepository fp) fn = fp FilePath.</> toFilePath fn

inRepoURL :: Repository r => r -> InRepoFileName -> URL
inRepoURL r fn = repoURL r Network.</> fn

putMegaPatches :: LockedRepository -> ByteString
               -> IO (InRepoFileName, Bytes)
putMegaPatches r s
    = do let fp = inRepoPath r patchFile
         h <- openBinaryFile fp AppendMode
         startSize <- hFileSizeBytes h
         BS.hPut h s
         hClose h
         return (patchFile, startSize)

getMegaPatchesWithInventory :: GeneralFlags
                            -> LockedRepository -> InRepoInventory
                            -> IO ByteString
getMegaPatchesWithInventory gf (LockedRepository fp) is
    = getContents gf (inventoryToContents fp is)

getMegaPatchesWithAbsoluteInventory :: GeneralFlags -> AbsoluteInventory
                                    -> IO ByteString
getMegaPatchesWithAbsoluteInventory gf inv
    = getContents gf (absoluteInventoryToContents inv)

readMegaPatchesWithAbsoluteInventory :: GeneralFlags -> AbsoluteInventory
                                     -> IO (Seq MegaPatch from to)
readMegaPatchesWithAbsoluteInventory gf inv
    = do bs <- getMegaPatchesWithAbsoluteInventory gf inv
         case input bs of
             (Stream2 ps, _{- "" -}) -> return ps

getContents :: GeneralFlags -> [Content FilePath] -> IO ByteString
getContents gf cs = getContents' gf $ simplifyContents cs

getContents' :: GeneralFlags -> [Content FilePath] -> IO ByteString
getContents' _  [] = return BS.empty
getContents' gf (c : cs) = do x <- getContent gf c
                              xs <- unsafeInterleaveIO $ getContents' gf cs
                              return (x `BS.append` xs)

-- XXX This is currently lazy. Really we want control over whether or
-- not it is strict
getContent :: GeneralFlags -> Content FilePath -> IO ByteString
getContent gf (Content fp from len)
    = do msg gf Verbose ("Getting content: file " ++ show fp
                                   ++ ", offset " ++ show from
                                   ++ ", length " ++ show len)
         h <- openBinaryFile fp ReadMode
         hSeekBytes h AbsoluteSeek from
         content <- hGetLazily (fromIntegral BS.defaultChunkSize) h len
         return content

readMegaPatches :: Repository r => GeneralFlags -> r
                -> IO (Seq MegaPatch from to)
readMegaPatches gf r = do content <- getMegaPatches gf r
                          case input content of
                              (Stream2 s, _{- "" -}) -> return s

{-
-- XXX Need a variant that checks validity, works with remote repos, etc
readMegaPatch :: Repository r => r -> InventoryItem -> IO (MegaPatch from to)
-- XXX We sometimes want this to be lazy, making chunks on demand with
-- unsafeInterleaveIO, and closing the handle when we get to "to".
-- e.g. when applying a patch.
-- However, when pulling patches we may need to do a merge, at which point
-- we want to be strict.
-- For now we are just always strict
readMegaPatch r (InventoryItem _ fn from len)
    = do h <- openBinaryFile (inRepoPath r fn) ReadMode
         hSeekBytes h AbsoluteSeek from
         content <- hGetBytes h len
         hClose h
         -- XXX check snd == ""?
         return $ fst $ input content
-}

writeMegaPatches :: LockedRepository -> Seq MegaPatch from to
                 -> IO [InRepoInventoryItem]
writeMegaPatches _ Nil = return []
writeMegaPatches r ps
    = do let fp = inRepoPath r patchFile
         h <- openBinaryFile fp AppendMode
         startSize <- hFileSizeBytes h
         is <- hWriteMegaPatches patchFile h startSize ps
         hClose h
         return is

hWriteMegaPatches :: InRepoFileName -> Handle -> Bytes -> Seq MegaPatch from to
                  -> IO [InRepoInventoryItem]
hWriteMegaPatches _ _ _ Nil = return []
hWriteMegaPatches pf h startSize (Cons p ps)
        = do (endSize, i) <- hWriteMegaPatch pf h startSize p
             is <- hWriteMegaPatches pf h endSize ps
             return (i : is)

writeMegaPatch :: LockedRepository -> MegaPatch from to
               -> IO InRepoInventoryItem
writeMegaPatch r m
    = do let fp = inRepoPath r patchFile
         h <- openBinaryFile fp AppendMode
         startSize <- hFileSizeBytes h
         (_, i) <- hWriteMegaPatch patchFile h startSize m
         hClose h
         return i

hWriteMegaPatch :: InRepoFileName -> Handle -> Bytes -> MegaPatch from to
                -> IO (Bytes, InRepoInventoryItem)
hWriteMegaPatch pf h startSize m@(MegaPatch n _ _)
    = do BS.hPut h $ output m
         endSize <- hFileSizeBytes h
         let len = endSize - startSize
         return (endSize, InventoryItem n pf startSize len)

applyToPristine :: Apply p => LockedRepository -> p from to -> IO ()
applyToPristine r ps = inDir (inRepoPath r pristineDir) $ applyFully ps

-- XXX This is wrong if there are local changes
applyToWorking :: Apply p => LockedRepository -> p from to -> IO ()
applyToWorking r ps = inDir (inRepoPath r workingDir) $ applyFully ps

-- XXX Assumes that working is empty (apart from _camp)
copyPristineToWorking :: LockedRepository -> IO ()
copyPristineToWorking r = copyTreeToDirectory (inRepoPath r pristineDir)
                                              (inRepoPath r workingDir)

mkRepoURL :: String -> IO URL
mkRepoURL xs | looksLikeURL xs = return (mkURL xs)
             | otherwise       = do fp <- canonicalizePath xs
                                    return (mkURL ("file://" ++ fp))

looksLikeURL :: String -> Bool
looksLikeURL xs = case break (\c -> c == '/' || c == ':') xs of
                  ([_], ':':_) -> False -- c:/path
                  (_, ':':_) -> True -- host:/path or proto://path
                  _ -> False

-- XXX This should check that it really is a repo
-- XXX This should also lock it
withLockedRepo :: FilePath -> (LockedRepository -> IO a) -> IO a
withLockedRepo fp f = do fp' <- canonicalizePath fp
                         f (LockedRepository fp')

withLockedRepoCreate :: (LockedRepository -> IO a) -> IO a
withLockedRepoCreate f = do -- XXX catch failure
                            createDirectory (toFilePath repoBase)
                            withLockedRepo "." f

withLockedRepoSearch :: (LockedRepository -> IO a) -> IO a
withLockedRepoSearch f = do fp <- searchForRepo
                            withLockedRepo fp f

-- XXX This should check that it really is a repo
withUnlockedRepo :: FilePath -> (UnlockedRepository -> IO a) -> IO a
withUnlockedRepo fp f
    = do inventoryRef <- newIORef Nothing
         patchesRef <- newIORef Nothing
         fp' <- mkRepoURL fp
         f (UnlockedRepository fp' inventoryRef patchesRef)
             `finally` do mInventoryFile <- readIORef inventoryRef
                          case mInventoryFile of
                              Nothing -> return ()
                              Just file -> removeFile file -- XXX catch error
                          mPatchesFile <- readIORef patchesRef
                          case mPatchesFile of
                              Nothing -> return ()
                              Just file -> removeFile file -- XXX catch error

withUnlockedRepoSearch :: (UnlockedRepository -> IO a) -> IO a
withUnlockedRepoSearch f = do fp <- searchForRepo
                              withUnlockedRepo fp f

searchForRepo :: IO FilePath
searchForRepo = do d <- getCurrentDirectory
                   f d
    where f d = do let fp = d FilePath.</> toFilePath repoBase
                   exists <- doesDirectoryExist fp
                   if exists
                       then return d
                       else do let d' = FilePath.takeDirectory d
                               if d' == d
                                   then -- XXX make a proper failure function
                                        do hPutStrLn stderr "You aren't in a repo!"
                                           exitFailure
                                   else f d'

-- createRepo has already created the actual repoBase
initialiseRepo :: LockedRepository -> IO ()
initialiseRepo r = do createDirectory (inRepoPath r logsDir)
                      createDirectory (inRepoPath r repoRoot)
                      createDirectory (inRepoPath r patchesDir)
                      createDirectory (inRepoPath r pristineDir)
                      writeInventory r []
                      writeAdds r []

-- XXX Should use the repo name and the patch metadata too
genName :: GeneralFlags -> LockedRepository -> IO Name
genName gf r
    = do TOD i j <- getClockTime -- XXX Error on i or j < 0?
         -- Avoid names already in our inventory
         inv <- readInventory gf r
         let ns = [ n | InventoryItem n _ _ _ <- inv ]
             mkName j' = Name Positive
                              (BSC.pack (showBase62 i ++ "-" ++ showBase62 j'))
             newName = head [ n
                            | j' <- [j..],
                              let n = mkName j',
                              n `notElem` ns ]
         return newName

showBase62 :: Integer -> String
showBase62 i = showIntAtBase 62 toBase62Digit i ""
    where toBase62Digit x = let y = fromIntegral x
                            in      if y < 10 then chr (ord '0' + y)
                               else if y < 36 then chr (ord 'a' + y - 10)
                               else                chr (ord 'A' + y - 36)

getAuthor :: LockedRepository -> IO (Maybe ByteString)
getAuthor r = do m1 <- maybeReadLine $ inRepoPath r authorFile
                 case m1 of
                     Nothing ->
                         do globalCampDir <- getAppUserDataDirectory "camp"
                            maybeReadLine (globalCampDir FilePath.</> "author")
                     j -> return j

-- XXX This should be in a Utils module
-- XXX Should really do this by catching exceptions
maybeReadLine :: FilePath -> IO (Maybe ByteString)
maybeReadLine fp = do exists <- doesFileExist fp
                      if exists then do h <- openFile fp ReadMode
                                        s <- hGetLine h
                                        hClose h
                                        return $ Just $ BSC.pack s
                                else return Nothing

nextLogFile :: InRepoFileName
nextLogFile = repoBase FN.</> fromString "nextLog"

logsDir :: InRepoFileName
logsDir = repoBase FN.</> fromString "logs"

copyRepo :: LockedRepository -> FilePath -> IO ()
copyRepo r to = copyTree (inRepoPath r repoRoot) to

