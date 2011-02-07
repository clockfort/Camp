
module Camp.Command.Pull (pull) where

import Camp.Command.Interactive
import Camp.Inventory
import Camp.Logging
import Camp.Messages
import Camp.Options
import Camp.Patch.Anonymous
import Camp.Patch.Commute
import Camp.Patch.Equality
import Camp.Patch.MegaPatch
import Camp.Patch.Merge
import Camp.Patch.Pretty
import Camp.Patch.Sequence
import Camp.Repository

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Set (Set)
import qualified Data.Set as Set

pull :: GeneralFlags -> [String] -> IO ()
pull gf (remoteRepoPath : wantedPatches)
    = withLockedRepoSearch $ \localRepo ->
      withUnlockedRepo remoteRepoPath $ \remoteRepo -> do
         msg gf Verbose "Opening log"
         withLog gf localRepo $ \_l -> do
             msg gf Verbose "Logging remote repo"
             -- XXX can't log unlocked repos ATM:
             -- logRepo l "remote" remoteRepo
             msg gf Verbose "Reading local inventory"
             localInventory <- readLocalInventory gf localRepo
             msg gf Verbose "Reading remote inventory"
             remoteInventory <- readLocalInventory gf remoteRepo
             -- XXX We've actually just downloaded the entire patchfile
             -- in order to do readLocalInventory, even though we're
             -- about to ignore (hopefully) most of it. Need to
             -- restructure to avoid that.
             let localNames = [ n | InventoryItem n _ _ _ <- localInventory ]
                 remoteNames = [ n | InventoryItem n _ _ _ <- remoteInventory ]
                 localNameSet  = Set.fromList localNames
                 remoteNameSet = Set.fromList remoteNames
                 commonNameSet = localNameSet `Set.intersection` remoteNameSet
                 -- We can skip over the prefix that is common to both repos
                 isCommon (InventoryItem n _ _ _) = n `Set.member` commonNameSet
                 localReadInventory = case span isCommon localInventory of
                                      (_, x) -> x
                 remoteReadInventory = case span isCommon remoteInventory of
                                       (_, x) -> x
             -- XXX Should special-case remote = {}

             -- We need to commute the remainder so that it it partitioned
             -- into the common patches, and those only in one of the repos
             localReadPatches <- readMegaPatchesWithAbsoluteInventory gf localReadInventory
             remoteReadPatches <- readMegaPatchesWithAbsoluteInventory gf remoteReadInventory
             case commuteToPrefix commonNameSet localReadPatches of
                 -- XXX Should special-case local = {}
                 _ `Then` localPatches ->
                     case commuteToPrefix commonNameSet remoteReadPatches of
                         _ `Then` remotePatches ->
                             case wantedPatches of
                             [] ->
                                 do res <- interactive describeMegaPatch remotePatches
                                    case res of
                                        remotePatches' `Then` _ ->
                                            doPull gf localRepo localPatches remotePatches'
                             ["-a"] ->
                                 do doPull gf localRepo localPatches remotePatches
                             _ ->
                                 let wantedShortDescs = map BSC.pack wantedPatches
                                     wantedNameSet = Set.fromList wantedShortDescs
                                 in case tryCommuteToPrefix wantedNameSet remotePatches of
                                    Just (remotePatches' `Then` _) ->
                                        doPull gf localRepo localPatches remotePatches'
                                    Nothing ->
                                        error "Can't pull those patches due to dependencies"
pull _ [] = error "XXX No arguments to pull"

describeMegaPatch :: MegaPatch from to -> String
describeMegaPatch (MegaPatch _ mi _) = pprint mi

-- GHC 6.8 can't cope if this is inlined, so we make it a separate function
-- And anyway, we now have two different uses of it.
doPull :: GeneralFlags -> LockedRepository
       -> Seq MegaPatch from1 to1 -> Seq MegaPatch from2 to2
       -> IO ()
doPull gf localRepo localPatches remotePatches =
    case sameStart localPatches remotePatches of
    IsEqual ->
        case merge (Fork localPatches remotePatches) of
        Anonymous1 newLocalPatches ->
            do -- XXX We're leaking space here
               msg gf Verbose "Writing patches"
               is <- writeMegaPatches localRepo newLocalPatches
               localInventory <- readInventory gf localRepo
               let localInventory' = localInventory ++ is
               msg gf Verbose "Applying patches to pristine"
               applyToPristine localRepo newLocalPatches
               msg gf Verbose "Applying patches to working"
               applyToWorking localRepo newLocalPatches
               msg gf Verbose "Writing inventory"
               writeInventory localRepo localInventory'

tryCommuteToPrefix :: Set ByteString -> Seq MegaPatch from to
                   -> Maybe (Then (Seq MegaPatch) (Seq MegaPatch) from to)
tryCommuteToPrefix _ Nil = Just (Nil `Then` Nil)
tryCommuteToPrefix ns (p@(MegaPatch _ (MetaInfo shortDesc _ _ _) _) `Cons` ps)
    = case tryCommuteToPrefix ns ps of
      Just (qs `Then` rs)
       | shortDesc `Set.member` ns ->
          Just (Cons p qs `Then` rs)
       | otherwise ->
          case commute (p `Then` qs) of
          Just (qs' `Then` p') ->
              Just (qs' `Then` Cons p' rs)
          Nothing -> Nothing
      Nothing -> Nothing

