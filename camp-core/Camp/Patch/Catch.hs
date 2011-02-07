
module Camp.Patch.Catch (Catch(..)) where

import Camp.Patch.Apply
import Camp.Patch.Commute
import Camp.Patch.ContextedPatch
import Camp.Patch.Equality
import Camp.Patch.InputOutput
import Camp.Patch.Invert
import Camp.Patch.Name
import Camp.Patch.Patch
import Camp.Patch.Pretty
import Camp.Patch.Sequence
import Camp.Utils

import qualified Data.ByteString.Lazy as BS
import Data.List
import qualified Data.Set as Set
import Unsafe.Coerce

data Catch from to where
    Patch :: Patch from to -> Catch from to
    Conflictor :: Seq Patch from to
               -> [ContextedPatch to]
               -> ContextedPatch to
               -> Catch from to

instance InputOutput (Catch from to) where
    input bs = case BS.head bs of
               0 -> case input (BS.tail bs) of
                    (p, bs') -> (Patch p, bs')
               1 -> case input (BS.tail bs) of
                    (effect, bs') ->
                        case input bs' of
                        (conflicts, bs'') ->
                            case input bs'' of
                            (identity, bs''') ->
                                (Conflictor effect conflicts identity, bs''')
               _ -> error "InputOutput Catch: Bad value"
    valid bs _ = case BS.uncons bs of
                 Just (0, bs') -> valid bs' (undefined :: Patch from to)
                 Just (1, bs') ->
                     case valid bs' (undefined :: Seq Patch from to) of
                     Left err -> Left err
                     Right bs'' ->
                         case valid bs'' (undefined :: [ContextedPatch to]) of
                         Left err -> Left err
                         Right bs''' ->
                             valid bs''' (undefined :: ContextedPatch to)
                 _ -> Left ("InputOutput Catch: Bad value", bs)
    output (Patch p) = 0 `BS.cons` output p
    output (Conflictor effect conflicts identity)
        = 1 `BS.cons` output effect
            `BS.append` output conflicts
            `BS.append` output identity

instance InputOutput2 Catch where
    input2 = input
    output2 = output
    valid2 = valid

instance Ppr (Catch from to) where
    ppr (Patch p) = text "Patch" <+> pprAtomic p
    ppr (Conflictor effect conflicts identity)
        = text "Conflictor"
       $$ nest 4 (pprAtomic effect)
       $$ nest 4 (pprAtomic conflicts)
       $$ nest 4 (pprAtomic identity)
    pprShow (Patch p) = text "Patch" <+> pprShowAtomic p
    pprShow (Conflictor effect conflicts identity)
        = text "Conflictor"
       $$ nest 4 (pprShowAtomic effect)
       $$ nest 4 (pprShowAtomic conflicts)
       $$ nest 4 (pprShowAtomic identity)

instance Equality Catch where
    -- XXX Should actually check the catches are equal!
    isEqual _ _ = unsafeCoerce IsEqual

instance Commute Catch Catch where
    commute (left `Then` right)
     = case (left `Then` right) of
       -- p [p^, {:p}, :q] <-> q [q^, {:q}, :p]
       (Patch p `Then` Conflictor qEffect [qConflict] qIdentity)
        | name p == name1 qConflict ->
           -- To sanity check, we need to confirm:
           --     p = qEffect^
           --     qConflict = : p
           --     qIdentity = : q
           case qEffect of
           Cons pInverse Nil ->
               let p' = invert pInverse
               in case sameStart p p' of
                  IsEqual ->
                      case isEqual p (invert pInverse) of
                      IsEqual ->
                          case qConflict of
                          ContextedPatch Nil p'' ->
                              case isEqual p p'' of
                              IsEqual ->
                                  case qIdentity of
                                  ContextedPatch Nil q ->
                                      Just (Patch q
                                            `Then`
                                            Conflictor (invert q `Cons` Nil)
                                                       [ContextedPatch Nil q]
                                                       (ContextedPatch Nil p))
                                  _ -> panic2 left right "Commute Catch Catch 1"
                          _ -> panic2 left right "Commute Catch Catch 2"
           _ -> panic2 left right "Commute Catch Catch 3"
       -- [p] [p^ r, {r^:p} U X, y] <-> [r, X, y] [, {y}, r^:p]
       -- XXX The qConflicts and qConflicts' type signatures are to fix
       -- building on GHC 6.8
       (Patch p `Then` Conflictor qEffect (qConflicts :: [ContextedPatch from]) qIdentity)
        | name p `elem` map name1 qConflicts ->
           -- To sanity check, we need to confirm:
           --     qEffect = p^ qEffect'
           --     (qEffect'^ : p) \in qConflicts
           --     qIdentity = : q
           case commuteToPrefix (Set.singleton (inverseSubName (name p))) qEffect of
           Cons pInv Nil `Then` qEffect' ->
               case isEqual (invert p) pInv of
               IsEqual ->
                   case partition ((name p ==) . name1) qConflicts of
                   ([ContextedPatch cxt p'], qConflicts' :: [ContextedPatch from]) ->
                       -- XXX I think this is wrong: Some of (invert qEffect')
                       -- might be able to commute through p?
                       case isEqual (invert qEffect') cxt of
                       IsEqual ->
                           case isEqual p p' of
                           IsEqual ->
                               let cp = addToContext (invert qEffect')
                                                     (ContextedPatch Nil p)
                               in Just (Conflictor qEffect'
                                                   qConflicts'
                                                   qIdentity
                                        `Then`
                                        Conflictor Nil
                                                   [qIdentity]
                                                   cp)
                   _ -> panic2 left right "Commute Catch Catch 4"
           _ -> panic2 left right "Commute Catch Catch 5"
       -- [r, X, y] [, {y}, r^:q] <-> [q] [q^r, {r^:q} U X, y]
       (Conflictor pEffect pConflicts pIdentity
        `Then`
        Conflictor qEffect [qConflict] qIdentity)
        | name1 pIdentity == name1 qConflict ->
           -- To sanity check, we need to confirm:
           --     qEffect = \epsilon
           --     qConflict = pIdentity
           --     qIdentity = pEffect^ : q
           case qEffect of
           Nil ->
                  -- XXX We need equality of contexted patches to be able to
                  -- do this:
                  -- case isEqual qConflict pIdentity of
                  -- IsEqual ->
                   case qIdentity of
                   ContextedPatch qCxt q ->
                       -- XXX I think this is wrong: Some of (invert pEffect)
                       -- might be able to commute through q?
                       case isEqual qCxt (invert pEffect) of
                       IsEqual ->
                           Just (Patch q
                                 `Then`
                                 Conflictor (invert q `Cons` pEffect)
                                            (qIdentity : pConflicts)
                                            pIdentity)
           _ -> panic2 left right "Commute Catch Catch 6"
       -- [r s, W, x] [t, {t^x} U Y, z] <-> [r t', s'Y, s'z] [s', z U t^W, t^x]
       (Conflictor pEffect pConflicts pIdentity
        `Then`
        Conflictor qEffect qConflicts qIdentity)
        | name1 pIdentity `elem` map name1 qConflicts ->
           -- -- XXX This sanity check comment is wrong:
           -- To sanity check, we need to confirm:
           --     qEffect = p^ qEffect'
           --     (qEffect'^ : p) \in qConflicts
           let pEffectNames = Set.fromList (names pEffect)
               qConflictsName = Set.fromList (map name1 qConflicts)
               commonFirstConflictNames = pEffectNames `Set.intersection`
                                          qConflictsName
           in case commuteToPrefix commonFirstConflictNames pEffect of
              commonEffects `Then` pOnlyEffect ->
                  case commute (pOnlyEffect `Then` qEffect) of
                  Just (qEffect' `Then` pEffect') ->
                      case partition ((name1 pIdentity ==) . name1) qConflicts of
                      ([ContextedPatch _cxt _p'], qConflicts') ->
                          -- XXX Check that
                          --         ContextedPatch cxt p'
                          --     is the same as pIdentity
                                  Just (Conflictor (commonEffects `appendSeq`
                                                    qEffect')
                                                   (map (addToContext pEffect') qConflicts')
                                                   (addToContext pEffect' qIdentity)
                                        `Then`
                                        Conflictor pEffect'
                                                   (qIdentity :
                                                    map (addToContext (invert qEffect)) pConflicts)
                                                   (addToContext (invert qEffect) pIdentity))
                      _ -> panic2 left right "Commute Catch Catch 7"
                  _ -> panic2 left right "Commute Catch Catch 8"

       -- From now on we know that the catches aren't conflictors that
       -- might conflict with each other

       -- patch/patch
       (Patch p `Then` Patch q) ->
           do (q' `Then` p') <- commute (p `Then` q)
              return (Patch q' `Then` Patch p')
       -- patch/conflictor
       (Patch p `Then` Conflictor qEffect qConflicts qIdentity) ->
           do qEffect' `Then` p' <- commute (p `Then` qEffect)
              let qConflicts' = map (addOneToContext p') qConflicts
              qIdentity' <- commutePast p' qIdentity
              return (Conflictor qEffect' qConflicts' qIdentity'
                      `Then`
                      Patch p')
       -- conflictor/patch
       (Conflictor pEffect pConflicts pIdentity `Then` Patch q) ->
           do q' `Then` pEffect' <- commute (pEffect `Then` q)
              let pConflicts' = map (addOneToContext (invert q)) pConflicts
              pIdentity' <- commutePast (invert q) pIdentity
              return (Patch q'
                      `Then`
                      Conflictor pEffect' pConflicts' pIdentity')
       -- conflictor/conflictor
       (Conflictor pEffect pConflicts pIdentity
        `Then`
        Conflictor qEffect qConflicts qIdentity) ->
           let pEffectNames = Set.fromList (names pEffect)
               qConflictsName = Set.fromList (map (inverseSubName . name1) qConflicts)
               commonFirstConflictNames = pEffectNames `Set.intersection`
                                          qConflictsName
           in case commuteToPrefix commonFirstConflictNames pEffect of
              commonEffects `Then` pOnlyEffect ->
                  do qEffect' `Then` pEffect' <- commute (pOnlyEffect `Then` qEffect)
                     pIdentity' <- allCommutePast (invert qEffect) pIdentity
                     qIdentity' <- allCommutePast pEffect' qIdentity
                     if -- any (\pConflict -> pConflict `conflictsWith` addToContext qEffect qIdentity) pConflicts ||
                        -- any (\qConflict -> pIdentity `conflictsWith` addToContext qEffect qConflict) qConflicts ||
                        (pIdentity `conflictsWith` addToContext qEffect qIdentity)
                         then Nothing
                         else return (Conflictor (commonEffects `appendSeq`
                                                  qEffect')
                                                 (map (addToContext pEffect') qConflicts)
                                                 qIdentity'
                                      `Then`
                                      Conflictor pEffect'
                                                 (map (addToContext (invert qEffect)) pConflicts)
                                                 pIdentity')

instance Invert Catch where
    invert (Patch p) = Patch (invert p)
    -- XXX Need to prove this right or introduce an InverseConflictor
    invert (Conflictor effect conflicts identity)
        = Conflictor (invert effect)
                     (map invertContextedPatch conflicts)
                     (invertContextedPatch identity)
        where invertContextedPatch (ContextedPatch ps p)
                  = addToContext effect
                  $ addToContext ps
                  $ ContextedPatch (Cons p Nil) (invert p)

instance Apply Catch where
    apply m (Patch p) = apply m p
    apply m (Conflictor effect _ _) = apply m effect

