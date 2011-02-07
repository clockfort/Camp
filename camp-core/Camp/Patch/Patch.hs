
module Camp.Patch.Patch (Patch(..)) where

import Camp.Patch.Apply
import Camp.Patch.Commute
import Camp.Patch.Equality
import Camp.Patch.InputOutput
import Camp.Patch.Invert
import Camp.Patch.Name
import Camp.Patch.Pretty
import Camp.Patch.Primitive

import qualified Data.ByteString.Lazy as BS
import Unsafe.Coerce

data Patch from to where
    Primitive :: SubName -> Primitive from to -> Patch from to

instance InputOutput (Patch from to) where
    input bs = case input bs of
               (sn, bs') ->
                   case input bs' of
                   (p, bs'') ->
                       (Primitive sn p, bs'')
    valid bs _ = case valid bs (undefined :: SubName) of
                 Left err -> Left err
                 Right bs' -> valid bs' (undefined :: Primitive from to)
    output (Primitive sn p) = output sn `BS.append` output p

instance InputOutput2 Patch where
    input2 = input
    output2 = output
    valid2 = valid

instance Ppr (Patch from to) where
    ppr (Primitive sn p) = text "Primitive" <+> pprAtomic sn
                        $$ nest 4 (pprAtomic p)
    pprShow (Primitive sn p) = text "Primitive" <+> pprShowAtomic sn
                            $$ nest 4 (pprShowAtomic p)

instance Equality Patch where
    -- XXX Should actually check the patches are equal!
    isEqual _ _ = unsafeCoerce IsEqual

instance Named Patch SubName where
    name (Primitive sn _) = sn

instance Commute Patch Patch where
    commute (Primitive np p `Then` Primitive nq q)
        = if np == inverseSubName nq
          then Nothing
          else do (q' `Then` p') <- commute (p `Then` q)
                  return (Primitive nq q' `Then` Primitive np p')

instance Invert Patch where
    invert (Primitive n p) = Primitive (inverseSubName n) (invert p)

instance Apply Patch where
    apply m (Primitive _ p) = apply m p

