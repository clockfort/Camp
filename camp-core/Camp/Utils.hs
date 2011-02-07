
module Camp.Utils (
    myLines, myUnlines, mySplitAt, maybeRead, maybeReads,
    stripPrefixBS, splitAtExactlyBS,
    hGetBytes, hGetLazily, hFileSizeBytes, hSeekBytes,
    readBinaryFile, writeBinaryFile, copyTree, copyTreeToDirectory, inDir,
    sanityCheck,
    panic, panic2, panic3,
    timeCommand,
    coerceToUnitContexts2,
    ) where

import Camp.Patch.Pretty
import Camp.Types

import Control.Exception
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Internal as BSI
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Int
import Data.List
import System.CPUTime
import System.Directory
import System.FilePath
import System.IO
import System.IO.Unsafe
import Unsafe.Coerce

-- XXX Can we use BSC.lines, or BSC.split '\n' instead?
-- The normal lines function returns the same thing for
--     lines "foo"
-- and
--     lines "foo\n"
myLines :: ByteString -> [ByteString]
myLines xs = case BSC.break ('\n' ==) xs of
             (ys, zs) ->
                 case BS.uncons zs of
                 Just (_, zs') -> ys : myLines zs'
                 Nothing -> [xs]

-- The normal unlines function always puts a trailing '\n' on
myUnlines :: [String] -> String
myUnlines xs = intercalate "\n" xs

mySplitAt :: Integer -> [a] -> Maybe ([a], [a])
mySplitAt i _ | i < 0 = error "mySplitAt: Negative number"
mySplitAt 0 xs = Just ([], xs)
mySplitAt _ [] = Nothing
mySplitAt i (x : xs) = case mySplitAt (i - 1) xs of
                       Just (ys, zs) -> Just (x : ys, zs)
                       Nothing -> Nothing

maybeRead :: Read a => String -> Maybe a
maybeRead xs = case maybeReads xs of
               Just (v, _) -> Just v
               Nothing -> Nothing

maybeReads :: Read a => String -> Maybe (a, String)
maybeReads xs = case reads xs of
                [(v, xs')] -> Just (v, xs')
                _ -> Nothing

stripPrefixBS :: ByteString -> ByteString -> Maybe ByteString
stripPrefixBS = f
    where f prefix bs = case BS.uncons prefix of
                        Nothing -> Just bs
                        Just (p, prefix') ->
                            case BS.uncons bs of
                            Just (b, bs')
                             | p == b ->
                                f prefix' bs'
                            _ -> Nothing

splitAtExactlyBS :: Int64 -> ByteString -> Maybe (ByteString, ByteString)
splitAtExactlyBS n0 bs0 = if n0 < 0
                          then Nothing
                          else f n0 bs0
    where f 0 bs = Just (BS.empty, bs)
          f _ BSI.Empty = Nothing
          f n (BSI.Chunk c cs) = let len = fromIntegral (SBS.length c)
                                 in if n >= len
                                    then case f (n - len) cs of
                                         Just (cs', bs') ->
                                             Just (BSI.Chunk c cs', bs')
                                         Nothing -> Nothing
                                    else -- XXX fromIntegral overflow possible
                                         case SBS.splitAt (fromIntegral n) c of
                                         (xs, ys) ->
                                             Just (BSI.Chunk xs BSI.Empty,
                                                   BSI.Chunk ys cs)

readBinaryFile :: FilePath -> IO String
readBinaryFile fp = withBinaryFile fp ReadMode $ \h -> do
                        xs <- hGetContents h
                        -- Hack to actually read in the contents:
                        _ <- evaluate (length xs)
                        return xs

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile fp xs = withBinaryFile fp WriteMode $ \h ->
                            hPutStr h xs

copyTree :: FilePath -> FilePath -> IO ()
copyTree from to = do createDirectory to
                      copyTreeToDirectory from to

copyTreeToDirectory :: FilePath -> FilePath -> IO ()
copyTreeToDirectory from to = do xs <- getDirectoryContents from
                                 mapM_ copyEntry xs
    where copyEntry "." = return ()
          copyEntry ".." = return ()
          copyEntry x = do let fromX = from </> x
                               toX = to </> x
                           file <- doesFileExist fromX
                           directory <- doesDirectoryExist fromX
                           case (file, directory) of
                               (True, True) -> panic "Both file and directory"
                               (True, False) -> copyFile fromX toX
                               (False, True) -> copyTree fromX toX
                               (False, False) -> panic "What is it?"

inDir :: FilePath -> IO a -> IO a
inDir dir io = do curDir <- getCurrentDirectory
                  (setCurrentDirectory dir >> io)
                      `finally` setCurrentDirectory curDir

sanityCheck :: String -> Bool -> a -> a
sanityCheck _ True x = x
sanityCheck str False _ = error ("Insanity: " ++ str)

panic :: String -> a
panic str = error ("Can't happen: " ++ str)

panic2 :: (Ppr p, Ppr q) => p -> q -> String -> a
panic2 p q str = panicn [str, pprint p, pprint q]

panic3 :: (Ppr p, Ppr q, Ppr r) => p -> q -> r -> String -> a
panic3 p q r str = panicn [str, pprint p, pprint q, pprint r]

panicn :: [String] -> a
panicn = panic . concat . intersperse "\n\n"

timeCommand :: String -> IO a -> IO a
timeCommand msg io
  = do putStr msg
       startCPUTime <- getCPUTime
       res <- io
       endCPUTime <- getCPUTime
       putStr $ take (maxLen - length msg) $ repeat '.'
       let timeTakenPicoSeconds = endCPUTime - startCPUTime
       let timeTakenSeconds = timeTakenPicoSeconds `div` (10 ^ (12 :: Int))
       print timeTakenSeconds
       return res
    where maxLen = 30 -- XXX Euch

-- XXX Need an audit of checking that sizes really are what we asked for
-- XXX Int can be smaller than bytes...
hGetBytes :: Handle -> Bytes -> IO ByteString
hGetBytes h len = hGetBytes h (fromIntegral len)

-- XXX Need an audit of checking that sizes really are what we asked for
hGetLazily :: Bytes -> Handle -> Bytes -> IO ByteString
hGetLazily _ h 0 = do -- XXX Should we be checking for EOF here?
                      hClose h
                      return BS.empty
hGetLazily chunkSize h n
    = do chunk <- BS.hGet h (fromIntegral (min chunkSize n))
         let n' = n - BS.length chunk
         n'' <- evaluate n'
         chunks <- unsafeInterleaveIO
                 $ hGetLazily chunkSize h n''
         return (chunk `BS.append` chunks)

hFileSizeBytes :: Handle -> IO Bytes
hFileSizeBytes h = do size <- hFileSize h
                      return $ fromIntegral size

hSeekBytes :: Handle -> SeekMode -> Bytes -> IO ()
hSeekBytes h sm to = hSeek h sm (fromIntegral to)

coerceToUnitContexts2 :: p a b -> p () ()
coerceToUnitContexts2 = unsafeCoerce

