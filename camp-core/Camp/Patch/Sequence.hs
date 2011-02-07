
module Camp.Patch.Sequence (
    Seq(..), names, commuteToPrefix, appendSeq, tryMerge
    ) where

import Camp.Patch.Anonymous
import Camp.Patch.Apply
import Camp.Patch.Commute
import Camp.Patch.Equality
import Camp.Patch.InputOutput
import Camp.Patch.Invert
import Camp.Patch.Name
import Camp.Patch.Pretty
import Camp.Types
import Camp.Utils

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Set (Set)
import qualified Data.Set as Set
import Unsafe.Coerce

infixr `Cons`

data Seq p from to where
    Cons :: p from mid -> Seq p mid to -> Seq p from to
    Nil :: Seq p here here

instance (Equality p, Commute p p, Named p n) => Equality (Seq p) where
    -- XXX Should actually check the sequences are equal!
    isEqual _ _ = unsafeCoerce IsEqual

names :: Named p n => Seq p from to -> [n]
names Nil = []
names (Cons p ps) = name p : names ps

data Cxt
contextHack :: p from to -> p Cxt Cxt
contextHack = unsafeCoerce

instance InputOutput2 p => InputOutput (Seq p from to) where
    input bs = case inputSeq bs of
               (s, bs') -> (unhide2 s, bs')
    valid bs _ = validSeq bs (undefined :: Proxy2 p)
    output Nil = BS.singleton 0
    output (x `Cons` xs) = 1 `BS.cons` output2 x `BS.append` output xs

-- This is written in such a way as to avoid leaking space due to
-- http://hackage.haskell.org/trac/ghc/ticket/2762
inputSeq :: InputOutput2 p => ByteString -> (Hide2 (Seq p), ByteString)
inputSeq bs = case BS.head bs of
              0 -> (hide2 Nil, BS.tail bs)
              1 -> case input2 (BS.tail bs) of
                   (x, bs') ->
                       case inputSeq bs' of
                       -- XXX There's a performance penalty for this ~,
                       -- but a significant space bonus
                       ~(xs, bs'') ->
                           (hide2 (x `Cons` unhide2 xs), bs'')
              _ -> error "InputOutput Seq: Bad value"

-- This is written in such a way as to avoid leaking space due to
-- http://hackage.haskell.org/trac/ghc/ticket/2762
validSeq :: forall p . InputOutput2 p
         => ByteString -> Proxy2 p
         -> Either (String, ByteString) ByteString
validSeq bs typeProxy = case BS.uncons bs of
                        Just (0, bs') -> Right bs'
                        Just (1, bs') ->
                            case valid2 bs' (undefined :: p from mid) of
                            Right bs'' ->
                                validSeq bs'' typeProxy
                            Left err -> Left err
                        _ -> Left ("InputOutput Seq: Bad value", bs)

instance Ppr (p Cxt Cxt) => Ppr (Seq p from to) where
    ppr Nil = text "Seq []"
    ppr xs = text "Seq ["
          $$ nest 4 (f (contextHack xs))
          $$ text "]"
        where f :: Seq p Cxt Cxt -> Doc
              f Nil = empty
              f (Cons p ps) = ppr (contextHack p) <> semi
                           $$ f (contextHack ps)
    pprAtomic = ppr
    pprShow Nil = text "Nil"
    pprShow (Cons x xs) = text "Cons"
                       $$ pprShowAtomic (contextHack x)
                       $$ pprShowAtomic (contextHack xs)
    pprShowAtomic s@Nil = pprShow s
    pprShowAtomic s = parens $ pprShow s

commutePastSequence :: Commute p q
                    => Then p (Seq q) from to
                    -> Maybe (Then (Seq q) p from to)
commutePastSequence (p `Then` Nil) = Just (Nil `Then` p)
commutePastSequence (p `Then` Cons q qs)
    = do (q' `Then` p') <- commute (p `Then` q)
         (qs' `Then` p'') <- commutePastSequence (p' `Then` qs)
         return (Cons q' qs' `Then` p'')

commuteSequencePast :: Commute p q
                    => Then (Seq p) q from to
                    -> Maybe (Then q (Seq p) from to)
commuteSequencePast (Nil `Then` q) = Just (q `Then` Nil)
commuteSequencePast (Cons p ps `Then` q)
    = do (q' `Then` ps') <- commuteSequencePast (ps `Then` q)
         (q'' `Then` p') <- commute (p `Then` q')
         return (q'' `Then` Cons p' ps')

instance Commute p q => Commute p (Seq q) where
    commute = commutePastSequence

instance Commute p q => Commute (Seq p) q where
    commute = commuteSequencePast

-- We need to provide this redundant instance or GHC won't know which
-- of the previous two instances to use for Commute (Seq Patch) (Seq Patch)
instance Commute p q => Commute (Seq p) (Seq q) where
    -- This can use either commutePastSequence or commuteSequencePast
    commute = commuteSequencePast

-- XXX This is a bit inefficient, as we don't normally actually want
-- the prefix.
-- We could also stop earlier if we kept track of how many unwanted
-- patches there were left
commuteToPrefix :: (Ord n, Commute p p, Named p n)
                => Set n -> Seq p from to
                -> Then (Seq p) (Seq p) from to
commuteToPrefix _ Nil = Nil `Then` Nil
commuteToPrefix ns ps = case tryCommuteToPrefix ns ps of
                        Just x -> x
                        Nothing -> panic "commuteToPrefix failed"

-- XXX The other use of this, in the pull command, got inlined and speciaised
-- as it now uses the short description rather than the patch name
tryCommuteToPrefix :: (Ord n, Commute p p, Named p n)
                   => Set n -> Seq p from to
                   -> Maybe (Then (Seq p) (Seq p) from to)
tryCommuteToPrefix _ Nil = Just (Nil `Then` Nil)
tryCommuteToPrefix ns (p `Cons` ps)
    = case tryCommuteToPrefix ns ps of
      Just (qs `Then` rs)
       | name p `Set.member` ns ->
          Just (Cons p qs `Then` rs)
       | otherwise ->
          case commute (p `Then` qs) of
          Just (qs' `Then` p') ->
              Just (qs' `Then` Cons p' rs)
          Nothing -> Nothing
      Nothing -> Nothing

instance Invert p => Invert (Seq p) where
    invert = f Nil
        where f :: Invert p => Seq p mid from -> Seq p mid to -> Seq p to from
              f is Nil = is
              f is (Cons p ps) = f (Cons (invert p) is) ps

instance Apply p => Apply (Seq p) where
    apply = applySeq

-- This is written in such a way as to avoid leaking space due to
-- http://hackage.haskell.org/trac/ghc/ticket/2762
applySeq :: Apply p => ApplyState -> Seq p from to -> IO ApplyState
applySeq m Nil = return m
applySeq m (Cons p ps) = do m' <- apply m p
                            applySeq m' ps

appendSeq :: Seq p from mid -> Seq p mid to -> Seq p from to
appendSeq Nil qs = qs
appendSeq (Cons p ps) qs = Cons p (appendSeq ps qs)

-- XXX Could use this in Catch merge, if we returned a bit more info
tryMerge :: (Named p n, Commute p p, Invert p)
         => Seq p from to1 -> Seq p from to2 -> Maybe (Anonymous1 (Seq p to1))
tryMerge ps qs
    = let pNames = names ps
          qNames = names qs
          pNameSet = Set.fromList pNames
          qNameSet = Set.fromList qNames
          commonNameSet = pNameSet `Set.intersection` qNameSet
      in case commuteToPrefix commonNameSet ps of
         _ `Then` psUnique ->
             case commuteToPrefix commonNameSet qs of
             _ `Then` qsUnique ->
                 case sameStart psUnique qsUnique of
                 IsEqual ->
                     case commute (invert psUnique `Then` qsUnique) of
                     Nothing ->
                         Nothing
                     Just (qsUnique' `Then` _) ->
                         Just (Anonymous1 qsUnique')

