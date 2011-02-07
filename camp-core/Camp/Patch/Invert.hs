
module Camp.Patch.Invert (Invert(..)) where

class Invert p where
    invert :: p from to -> p to from
