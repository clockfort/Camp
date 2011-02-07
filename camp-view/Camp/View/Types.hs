
module Camp.View.Types (Mapping, DepInfo) where

import Camp.Patch.Name

type Mapping from to = [(from, to)]

type DepInfo = (Bool,     -- Does no-one dep on me?
                [(Name,
                  Bool)]) -- Am I the last to dep on this?

