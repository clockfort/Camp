
module Camp.Command.Inventory (inventory) where

import Camp.Inventory
import Camp.Options
import Camp.Patch.Pretty
import Camp.Repository

import qualified Data.Set as Set

inventory :: GeneralFlags -> [String] -> IO ()
inventory gf [] = withLockedRepoSearch $ \r -> do
                      i <- readInventory gf r
                      let ns = [ n | InventoryItem n _ _ _ <- i ]
                      mapM_ (putStrLn . pprint) ns
inventory gf [remoteRepoPath]
    = withUnlockedRepoSearch $ \localRepo ->
      withUnlockedRepo remoteRepoPath $ \remoteRepo -> do
          localInventory  <- readInventory gf localRepo
          remoteInventory <- readInventory gf remoteRepo
          let localNames = [ n | InventoryItem n _ _ _ <- localInventory ]
              remoteNames = [ n | InventoryItem n _ _ _ <- remoteInventory ]
              localNameSet  = Set.fromList localNames
              remoteNameSet = Set.fromList remoteNames
              localOnly = localNameSet   `Set.difference` remoteNameSet
              remoteOnly = remoteNameSet `Set.difference` localNameSet
          putStrLn "Here only:"
          mapM_ (putStrLn . pprint) $ Set.toList localOnly
          putStrLn ""
          putStrLn "There only:"
          mapM_ (putStrLn . pprint) $ Set.toList remoteOnly
inventory _ _ = error "Bad arguments to inventory"

