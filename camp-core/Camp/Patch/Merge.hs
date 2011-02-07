
module Camp.Patch.Merge where

import Camp.Patch.Anonymous
import Camp.Patch.Catch
import Camp.Patch.Commute
import Camp.Patch.ContextedPatch
import Camp.Patch.Equality
import Camp.Patch.Invert
import Camp.Patch.MegaPatch
import Camp.Patch.Pretty
import Camp.Patch.Sequence
import Camp.Utils

import qualified Data.Set as Set

data Fork p q to1 to2 where
    Fork :: p from to1 -> q from to2 -> Fork p q to1 to2

class Commute p q => Merge p q where
    merge :: Fork p q mid1 mid2 -> Anonymous1 (q mid1)

instance Merge Catch Catch where
    merge (Fork c1 c2)
        = case commute (invert c1 `Then` c2) of
          Just (c2' `Then` _) ->
              Anonymous1 c2'
          Nothing ->
              case (c1, c2) of
              (Patch p, Patch q) ->
                  let pConflict = ContextedPatch Nil p
                      qIdentity = ContextedPatch Nil q
                  in Anonymous1 $ Conflictor (invert p `Cons` Nil)
                                             [pConflict]
                                             qIdentity
              (Conflictor pEffect _ pIdentity, Patch q) ->
                  let qIdentity = addToContext (invert pEffect)
                                $ ContextedPatch Nil q
                  in Anonymous1 (Conflictor Nil [pIdentity] qIdentity)
              (Patch p, Conflictor qEffect qConflicts qIdentity) ->
                  let pConflict = addToContext (invert qEffect)
                                $ ContextedPatch Nil p
                  in Anonymous1 $ Conflictor (invert p `Cons` qEffect)
                                             (pConflict : qConflicts)
                                             qIdentity
              (Conflictor pEffect _ pIdentity,
               Conflictor qEffect qConflicts qIdentity) ->
                  let pEffectNames = names pEffect
                      qEffectNames = names qEffect
                      pEffectNameSet = Set.fromList pEffectNames
                      qEffectNameSet = Set.fromList qEffectNames
                      commonEffectNameSet = pEffectNameSet `Set.intersection`
                                            qEffectNameSet
                  in case commuteToPrefix commonEffectNameSet pEffect of
                     _ `Then` pEffect' ->
                         case commuteToPrefix commonEffectNameSet qEffect of
                         _ `Then` qEffect' ->
                             case sameStart pEffect' qEffect' of
                             IsEqual ->
                                 case commute (invert pEffect' `Then` qEffect') of
                                 Nothing ->
                                     panic ("catch merge commute\n\n"
                                         ++ "catch 1:\n"
                                         ++ pprint c1
                                         ++ "\n\n"
                                         ++ "catch 2:\n"
                                         ++ pprint c2
                                         ++ "\n\n"
                                         ++ "effect 1:\n"
                                         ++ pprint pEffect'
                                         ++ "\n\n"
                                         ++ "effect 2:\n"
                                         ++ pprint qEffect'
                                         ++ "\n")
                                 Just (qEffect'' `Then` ipEffect'') ->
                                     let qConflicts' = map (addToContext ipEffect'') qConflicts
                                         qConflicts'' = addToContext (invert qEffect'') pIdentity
                                                      : qConflicts'
                                         qIdentity' = addToContext ipEffect'' qIdentity
                                     in Anonymous1 (Conflictor qEffect'' qConflicts'' qIdentity')

instance (Equality q, Merge p q, Ppr (p () ()), Ppr (q () ())) => Merge p (Seq q) where
    merge (Fork _ Nil) = Anonymous1 Nil
    merge (Fork p (Cons q qs))
        = case merge (Fork p q) of
          Anonymous1 q' ->
              case commute (p `Then` q') of
                  Just (q'' `Then` p') ->
                      case isEqual q q'' of
                      IsEqual ->
                          case merge (Fork p' qs) of
                          Anonymous1 qs' ->
                              Anonymous1 (Cons q' qs')
                  Nothing -> panic3 (coerceToUnitContexts2 p)
                                    (coerceToUnitContexts2 q)
                                    (coerceToUnitContexts2 q')
                                    "sequence merge commute"

instance (Equality q, Merge p q, Ppr (p () ()), Ppr (q () ())) => Merge (Seq p) (Seq q) where
    merge (Fork Nil qs) = Anonymous1 qs
    merge (Fork (Cons p ps) qs) = case merge (Fork p qs) of
                                  Anonymous1 qs' ->
                                      merge (Fork ps qs')

instance Merge MegaPatch MegaPatch where
    merge (Fork (MegaPatch _ _ ps) (MegaPatch nq miq qs))
        = case merge (Fork ps qs) of
          Anonymous1 qs' ->
              Anonymous1 (MegaPatch nq miq qs')

