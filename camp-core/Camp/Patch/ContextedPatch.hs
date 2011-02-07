
module Camp.Patch.ContextedPatch (
    ContextedPatch(..), addToContext, addOneToContext, conflictsWith,
    allCommutePast, commutePast
    ) where

import Camp.Patch.Commute
import Camp.Patch.Equality
import Camp.Patch.InputOutput
import Camp.Patch.Invert
import Camp.Patch.Name
import Camp.Patch.Patch
import Camp.Patch.Pretty
import Camp.Patch.Sequence

import qualified Data.ByteString.Lazy as BS
import Data.Maybe

data ContextedPatch from where
    ContextedPatch :: Seq Patch from mid -> Patch mid to
                   -> ContextedPatch from

instance InputOutput (ContextedPatch from) where
    input bs = case input bs of
               (ps, bs') ->
                   case input bs' of
                   (p, bs'') ->
                       (ContextedPatch ps p, bs'')
    valid bs _ = case valid bs (undefined :: Seq Patch from mid) of
                 Left err -> Left err
                 Right bs' -> valid bs' (undefined :: Patch mid to)
    output (ContextedPatch ps p) = output ps `BS.append` output p

instance Named1 ContextedPatch SubName where
    name1 (ContextedPatch _ p) = name p

instance Ppr (ContextedPatch from) where
    ppr (ContextedPatch ps p) = pprAtomic ps <+> colon <+> pprAtomic p
    pprShow (ContextedPatch ps p) = text "ContextedPatch"
                                 $$ nest 4 (pprShowAtomic ps)
                                 $$ nest 4 (pprShowAtomic p)

commutePast :: Patch from mid -> ContextedPatch mid
            -> Maybe (ContextedPatch from)
commutePast p (ContextedPatch qs q)
    = do (qs' `Then` p') <- commute (p `Then` qs)
         (q' `Then` _) <- commute (p' `Then` q)
         return (ContextedPatch qs' q')

allCommutePast :: Seq Patch from mid -> ContextedPatch mid
               -> Maybe (ContextedPatch from)
allCommutePast ps (ContextedPatch qs q)
    = do (qs' `Then` ps') <- commute (ps `Then` qs)
         (q' `Then` _) <- commute (ps' `Then` q)
         return (ContextedPatch qs' q')

commutePastOrAnnihilate :: Patch from mid -> ContextedPatch mid
                        -> Maybe (ContextedPatch from)
commutePastOrAnnihilate p (ContextedPatch qs q)
    = do cpa <- commutePastSeqOrAnnihilate (p `Then` qs)
         case cpa of
             CommutedPast (qs' `Then` p') ->
                 do (q' `Then` _) <- commute (p' `Then` q)
                    return (ContextedPatch qs' q')
             Annihilated qs' ->
                 return (ContextedPatch qs' q)

commutePastSeqOrAnnihilate :: Then Patch (Seq Patch) from to
                           -> Maybe (CommutedPastOrAnnihilated from to)
commutePastSeqOrAnnihilate (p `Then` Nil) = Just (CommutedPast (Nil `Then` p))
commutePastSeqOrAnnihilate (p `Then` Cons q qs)
    = if inverseSubName (name p) == name q
      then case isEqual (invert p) q of
           IsEqual -> Just (Annihilated qs)
      else do (q' `Then` p') <- commute (p `Then` q)
              cpa <- commutePastSeqOrAnnihilate (p' `Then` qs)
              case cpa of
                  CommutedPast (qs' `Then` p'') ->
                      return (CommutedPast (Cons q' qs' `Then` p''))
                  Annihilated qs' ->
                      return (Annihilated (Cons q' qs'))

data CommutedPastOrAnnihilated from to where
    CommutedPast :: Then (Seq Patch) Patch from to
                 -> CommutedPastOrAnnihilated from to
    Annihilated :: Seq Patch from to -> CommutedPastOrAnnihilated from to

addToContext :: Seq Patch from mid -> ContextedPatch mid -> ContextedPatch from
addToContext Nil cq = cq
addToContext (Cons p ps) cq
    = addOneToContext p (addToContext ps cq)

addOneToContext :: Patch from mid -> ContextedPatch mid -> ContextedPatch from
addOneToContext p cq@(ContextedPatch qs q)
    = case commutePastOrAnnihilate p cq of
      Nothing -> ContextedPatch (Cons p qs) q
      Just cq' -> cq'

conflictsWith :: ContextedPatch from -> ContextedPatch from -> Bool
conflictsWith (ContextedPatch ps p) (ContextedPatch qs q)
    = let pSeq = appendSeq ps (Cons p Nil)
          qSeq = appendSeq qs (Cons q Nil)
      in not (isJust (tryMerge pSeq qSeq))

