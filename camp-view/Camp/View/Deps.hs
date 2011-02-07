
{-
XXX This program is just a quick hack. It can probably be written much
more efficiently, and much more nicely
-}

module Camp.View.Deps (findMarkedDeps) where

import Camp.Patch.Commute
import Camp.Patch.MegaPatch
import Camp.Patch.Name
import Camp.Patch.Sequence
import Camp.Patch.RevSequence

import Camp.View.Types

import Data.List
import qualified Data.Set as Set

-- XXX Lots of reverse...
findMarkedDeps :: Seq MegaPatch from to -> Mapping Name DepInfo
findMarkedDeps = reverse . markLastDeps . reverse . findAllDeps

findAllDeps :: Seq MegaPatch from to -> Mapping Name [Name]
findAllDeps = f NilRevSeq
    where f :: RevSeq MegaPatch from mid -> Seq MegaPatch mid to
            -> Mapping Name [Name]
          f _ Nil = []
          f past (p `Cons` ps) = (name p, findDeps past p)
                               : f (past `Snoc` p) ps

findDeps :: RevSeq MegaPatch from mid -> MegaPatch mid to -> [Name]
findDeps NilRevSeq _ = []
findDeps (ps `Snoc` p) me = case commute (p `Then` me) of
                            Just (me' `Then` _) -> findDeps ps me'
                            Nothing ->
                                case commuteOut ps p of
                                HiddenFrom ps' ->
                                    name p : findDeps ps' me

-- XXX This should be in core
data HiddenFrom p to
    where HiddenFrom :: p from to -> HiddenFrom p to

commuteOut :: RevSeq MegaPatch from mid -> MegaPatch mid to
           -> HiddenFrom (RevSeq MegaPatch) to
commuteOut NilRevSeq _ = HiddenFrom NilRevSeq
commuteOut (ps `Snoc` p) me = case commute (p `Then` me) of
                              Just (me' `Then` p') ->
                                  case commuteOut ps me' of
                                  HiddenFrom ps' ->
                                      HiddenFrom (ps' `Snoc` p')
                              Nothing ->
                                  case commuteOut ps p of
                                  HiddenFrom ps' ->
                                      commuteOut ps' me

markLastDeps :: Mapping Name [Name]
             -> Mapping Name DepInfo
markLastDeps = f Set.empty
    where f _ [] = []
          f seenNames ((patchName, depNames) : xs)
              = let allNames = patchName : depNames
                    noRevDeps = patchName `Set.notMember` seenNames
                    depNames' = [ (n, n `Set.notMember` seenNames)
                                | n <- depNames ]
                    seenNames' = foldl' (flip Set.insert) seenNames allNames
                in (patchName, (noRevDeps, depNames')) : f seenNames' xs

