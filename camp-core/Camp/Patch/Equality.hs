
module Camp.Patch.Equality (
    Equality(..), IsEqual(..), sameStart, sameEnd, startIsEnd
    ) where

import Unsafe.Coerce

class Equality p where
    isEqual :: p from to1 -> p from to2 -> IsEqual to1 to2

data IsEqual a b where
    IsEqual :: IsEqual a a

sameStart :: p from1 to1 -> q from2 to2 -> IsEqual from1 from2
sameStart _ _ = unsafeCoerce IsEqual

sameEnd :: p from1 to1 -> q from2 to2 -> IsEqual to1 to2
sameEnd _ _ = unsafeCoerce IsEqual

startIsEnd :: p from to -> IsEqual from to
startIsEnd _ = unsafeCoerce IsEqual

