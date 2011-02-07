
module Camp.Patch.Stream (Stream(..), Stream2(..)) where

import Camp.Patch.InputOutput
import Camp.Patch.Sequence
import Camp.Types

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Unsafe.Coerce

newtype Stream a = Stream [a]

instance InputOutput a => InputOutput (Stream a) where
    input bs = (Stream (f bs), BS.empty)
        where f bs0 = if BS.null bs0
                      then []
                      else case input bs0 of
                           (x, bs1) ->
                               x : f bs1
    valid bs _ = validStream bs (undefined :: Proxy a)
    output (Stream []) = BS.empty
    output (Stream (x : xs)) = output x `BS.append` output (Stream xs)

-- This is written in such a way as to avoid leaking space due to
-- http://hackage.haskell.org/trac/ghc/ticket/2762
validStream :: forall a . InputOutput a
            => ByteString -> Proxy a
            -> Either (String, ByteString) ByteString
validStream bs typeProxy = if BS.null bs
                           then Right BS.empty
                           else case valid bs (undefined :: a) of
                                Right bs'' ->
                                    validStream bs'' typeProxy
                                Left err -> Left err

-----

newtype Stream2 p from to = Stream2 (Seq p from to)

instance InputOutput2 p => InputOutput (Stream2 p from to) where
    input bs = (Stream2 (f bs), BS.empty)
        where f bs0 = if BS.null bs0
                      then unsafeCoerce Nil
                      else case input2 bs0 of
                           (x, bs1) ->
                               x `Cons` f bs1
    valid bs _ = validStream2 bs (undefined :: Proxy2 p)
    output (Stream2 Nil) = BS.empty
    output (Stream2 (x `Cons` xs)) = output2 x `BS.append` output (Stream2 xs)

-- This is written in such a way as to avoid leaking space due to
-- http://hackage.haskell.org/trac/ghc/ticket/2762
validStream2 :: forall p . InputOutput2 p
             => ByteString -> Proxy2 p
             -> Either (String, ByteString) ByteString
validStream2 bs typeProxy = if BS.null bs
                            then Right BS.empty
                            else case valid2 bs (undefined :: p from mid) of
                                 Right bs'' ->
                                     validStream2 bs'' typeProxy
                                 Left err -> Left err

