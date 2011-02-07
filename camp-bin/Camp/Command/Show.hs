
module Camp.Command.Show (showC) where

import Camp.Inventory
import Camp.Options
import Camp.Patch.Name
import Camp.Patch.Pretty
import Camp.Repository

showC :: GeneralFlags -> [String] -> IO ()
showC gf [wanted]
    = withUnlockedRepoSearch $ \r -> do
         inventory <- readLocalInventory gf r
         let wantedName = case parseName wanted of
                          Just n -> n
                          Nothing ->
                              -- This is a bit of a hack; if they give
                              -- an unsigned name, assume they want the
                              -- positive one
                              case parseName ('P':'-':wanted) of
                              Just n -> n
                              Nothing -> error "Bad Name"
             isWanted (InventoryItem n _ _ _) = n == wantedName
             inventoryLine = case filter isWanted inventory of
                             [i] -> i
                             _ -> error "XXX Can't find patch"
         patches <- readMegaPatchesWithAbsoluteInventory gf [inventoryLine]
         putStrLn $ pprint patches
showC _ _ = error "XXX show"

