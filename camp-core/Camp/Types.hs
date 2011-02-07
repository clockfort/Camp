
module Camp.Types (Bytes, Line, Proxy, Proxy2, Hide2, hide2, unhide2) where

import Data.Int
import Unsafe.Coerce

-- We pick the types of Bytes to match the type that
-- Data.ByteString.Lazy uses.
type Bytes = Int64
-- If we have more lines than bytes then we're in trouble,
-- so we might as well use the same type here too.
type Line = Int64

data Proxy p
data Proxy2 (p :: * -> * -> *)

data Hide2 t where
    Hide2 :: t a b -> Hide2 t

hide2 :: t a b -> Hide2 t
hide2 x = Hide2 x

unhide2 :: Hide2 t -> t a b
unhide2 (Hide2 x) = unsafeCoerce x

