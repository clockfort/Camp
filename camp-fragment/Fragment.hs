
module Main (main) where

import Camp.Content
import Camp.Inventory
import Camp.Options
import Camp.Repository

import qualified Data.ByteString.Lazy.Char8 as BSC
import System.Directory
import System.Environment
import System.IO.Unsafe
import System.Random

main :: IO ()
main = do args <- getArgs
          case args of
              [compactRepoDir, fragmentedRepoDir] ->
                  withLockedRepo compactRepoDir $ \compactRepo ->
                  do createDirectory fragmentedRepoDir
                     setCurrentDirectory fragmentedRepoDir
                     withLockedRepoCreate $ \fragmentedRepo ->
                         do initialiseRepo fragmentedRepo
                            fragment compactRepo fragmentedRepo
              _ -> error "Bad args"

fragment :: LockedRepository -> LockedRepository -> IO ()
fragment compactRepo fragmentedRepo
 = do compactInv <- readInventory gf compactRepo
      writeRandom
      fragmentedInv <- f compactInv
      writeInventory fragmentedRepo fragmentedInv
    where gf = GeneralFlags {
                   gfVerbosity = Normal,
                   gfLog = False
               }
          writeRandom = do n <- randomRIO (5000, 6000)
                           let n' = fromInteger n
                           putMegaPatches fragmentedRepo (BSC.replicate n' 'x')
                           return ()
          f [] = return []
          f (InventoryItem name fn from len : is)
              = do let c = Content (inRepoPath compactRepo fn) from len
                   bs <- getContent gf c
                   (fn', from') <- putMegaPatches fragmentedRepo bs
                   writeRandom
                   is' <- unsafeInterleaveIO $ f is
                   return (InventoryItem name fn' from' len : is')

