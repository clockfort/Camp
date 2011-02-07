
module Camp.Patch.Commute (Then(..), Commute(..)) where

data Then p q from to where
    Then :: p from mid -> q mid to -> Then p q from to

class Commute p q where
    commute :: Then p q from to -> Maybe (Then q p from to)

