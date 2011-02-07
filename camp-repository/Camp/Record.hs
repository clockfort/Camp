
module Camp.Record (recordMegaPatch) where

import Camp.Patch.Catch
import Camp.Patch.MegaPatch
import Camp.Patch.Name
import Camp.Patch.Patch
import Camp.Patch.Primitive
import Camp.Patch.Sequence
import Camp.Repository

recordMegaPatch :: LockedRepository -> Name -> MetaInfo
                -> Seq Primitive from to
                -> IO ()
recordMegaPatch r n mi primitives = do
    let patches = mkPatches n 1 primitives
        catches = mkCatches patches
        megaPatch = MegaPatch n mi catches
    i <- writeMegaPatch r megaPatch
    applyToPristine r megaPatch
    appendInventoryItem r i

mkPatches :: Name -> Integer -> Seq Primitive from to -> Seq Patch from to
mkPatches _ _ Nil = Nil
mkPatches n i (Cons p ps) = Primitive (SubName n i) p
                            `Cons`
                            mkPatches n (i + 1) ps

mkCatches :: Seq Patch from to -> Seq Catch from to
mkCatches Nil = Nil
mkCatches (Cons p ps) = Patch p `Cons` mkCatches ps

