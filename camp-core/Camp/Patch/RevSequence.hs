
module Camp.Patch.RevSequence (RevSeq(..), toSeq) where

import Camp.Patch.Commute
import Camp.Patch.Sequence

infixr `Snoc`

data RevSeq p from to where
    Snoc :: RevSeq p from mid -> p mid to -> RevSeq p from to
    NilRevSeq :: RevSeq p here here

toSeq :: RevSeq p from to -> Seq p from to
toSeq rs = toSeq' Nil rs

toSeq' :: Seq p mid to -> RevSeq p from mid -> Seq p from to
toSeq' acc NilRevSeq = acc
toSeq' acc (rs `Snoc` p) = toSeq' (p `Cons` acc) rs

commutePastRevSequence :: Commute p q
                    => Then p (RevSeq q) from to
                    -> Maybe (Then (RevSeq q) p from to)
commutePastRevSequence (p `Then` NilRevSeq) = Just (NilRevSeq `Then` p)
commutePastRevSequence (p `Then` Snoc qs q)
    = do (qs' `Then` p') <- commutePastRevSequence (p `Then` qs)
         (q' `Then` p'') <- commute (p' `Then` q)
         return (Snoc qs' q' `Then` p'')

commuteRevSequencePast :: Commute p q
                    => Then (RevSeq p) q from to
                    -> Maybe (Then q (RevSeq p) from to)
commuteRevSequencePast (NilRevSeq `Then` q) = Just (q `Then` NilRevSeq)
commuteRevSequencePast (Snoc ps p `Then` q)
    = do (q' `Then` p') <- commute (p `Then` q)
         (q'' `Then` ps') <- commuteRevSequencePast (ps `Then` q')
         return (q'' `Then` Snoc ps' p')

instance Commute p q => Commute p (RevSeq q) where
    commute = commutePastRevSequence

instance Commute p q => Commute (RevSeq p) q where
    commute = commuteRevSequencePast

-- We need to provide this redundant instance or GHC won't know which
-- of the previous two instances to use for
-- Commute (RevSeq Patch) (RevSeq Patch)
instance Commute p q => Commute (RevSeq p) (RevSeq q) where
    -- This can use either commutePastRevSequence or commuteRevSequencePast
    commute = commuteRevSequencePast

