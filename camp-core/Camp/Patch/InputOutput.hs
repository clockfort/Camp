
module Camp.Patch.InputOutput (
    InputOutput(..), InputOutput2(..), thenValid
    ) where

import Camp.Utils

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Bits
import Data.Int
import Data.List
import Data.Word
import System.Time

class InputOutput a where
    input :: ByteString -> (a, ByteString)
    -- XXX We should probably do the ShowS trick, but this'll do for now.
    -- Our structures are shallow, after all.
    output :: a -> ByteString

    -- XXX This is a bit of a mess, because we don't get any help from
    -- the type checker as to what recursive calls we need to make.
    -- The idea is that (read >>= validate; read >>= input >>= apply)
    -- should be able to run in constant space, and input cannot fail
    -- it validate succeeded. And in fact, validate actually does even
    -- more checking than that, e.g. it checks that filenames are
    -- actually valid, that names only contain valid name character,
    -- etc.

    -- "valid bs undefined" should return True if the bytestring
    -- can be successfully read as a value of type a
    valid :: ByteString -> a -> Either (String,     -- Error
                                        ByteString) -- where the error happened
                                        ByteString  -- remainder of the input

thenValid :: InputOutput a => Either (String, ByteString) ByteString -> a
                           -> Either (String, ByteString) ByteString
Left err `thenValid` _ = Left err
Right bs `thenValid` x = valid bs x

class InputOutput2 p where
    input2 :: ByteString -> (p from to, ByteString)
    output2 :: p from to -> ByteString
    valid2 :: ByteString -> p from to -> Either (String, ByteString) ByteString

instance InputOutput ByteString where
    input bs0 = case input bs0 of
                (w, bs1) ->
                    case splitAtExactlyBS w bs1 of
                    Just (bs2, bs3) -> (bs2, bs3)
                    Nothing -> error "input ByteString: Too short"
    valid bs _ = case valid bs (undefined :: Word64) of
                 Left err -> Left err
                 Right _ ->
                     case input bs of
                     (w, bs') ->
                         case splitAtExactlyBS (fromIntegral (w :: Word64)) bs' of
                         Just (_, bs'') -> Right bs''
                         Nothing ->
                             Left ("InputOutput ByteString truncated", bs)
    output bs = output (BS.length bs) `BS.append` bs

instance InputOutput Int64 where
    input bs = case input bs of
               (w, bs') ->
                   let i = fromIntegral (w :: Word64)
                   in i `seq` (i, bs')
    valid bs _ = valid bs (undefined :: Word64)
    output i = output (fromIntegral i :: Word64)

instance InputOutput Word64 where
    input bs = case splitAtExactlyBS 8 bs of
               Just (xs, bs') ->
                   let w = foldl1' f $ map fromIntegral $ BS.unpack xs
                   in w `seq` (w, bs')
               Nothing -> error "input Word64: Not long enough"
        where f x y = (x `shiftL` 8) .|. y
    valid bs _ = case splitAtExactlyBS 8 bs of
                 Just (_, bs') -> Right bs'
                 Nothing -> Left ("InputOutput Word64 not enough bytes", bs)
    output w = BS.pack [fromIntegral (w `shiftR` 56),
                        fromIntegral (w `shiftR` 48),
                        fromIntegral (w `shiftR` 40),
                        fromIntegral (w `shiftR` 32),
                        fromIntegral (w `shiftR` 24),
                        fromIntegral (w `shiftR` 16),
                        fromIntegral (w `shiftR` 8),
                        fromIntegral w]

instance InputOutput Integer where
    input bs = case BS.head bs of
               0 -> case input (BS.tail bs) of
                    (i, bs') ->
                        let i' = fromIntegral (i :: Int64)
                        in i' `seq` (i', bs')
               1 -> case input (BS.tail bs) of
                    (ws, bs') ->
                        let i = fromWord64s ws
                        in i `seq` (i, bs')
               2 -> case input (BS.tail bs) of
                    (ws, bs') ->
                        let i = negate $ fromWord64s ws
                        in i `seq` (i, bs')
               _ -> error "InputOutput Integer: Bad value"
        where fromWord64s :: [Word64] -> Integer
              fromWord64s ws = foldr1 f $ map fromIntegral ws
              f x y = (x `shiftL` 64) .|. y
    valid bs _ = case BS.uncons bs of
                 Just (0, bs') -> valid bs' (undefined :: Int64)
                 Just (1, bs') -> valid bs' (undefined :: [Word64])
                 Just (2, bs') -> valid bs' (undefined :: [Word64])
                 _ -> Left ("InputOutput Integer: Bad value", bs)
    output i = if (i <= fromIntegral (maxBound :: Int64)) &&
                  (i >= fromIntegral (minBound :: Int64))
               then 0 `BS.cons` output (fromIntegral i :: Int64)
               else if i > 0
               then 1 `BS.cons` output (toWord64s i)
               else 2 `BS.cons` output (toWord64s (negate i))
        where toWord64s :: Integer -> [Word64]
              toWord64s 0 = []
              toWord64s j = fromIntegral j : toWord64s (j `shiftR` 64)

instance InputOutput ClockTime where
    input bs0 = case input bs0 of
                (i, bs1) ->
                    case input bs1 of
                    (j, bs2) ->
                        (TOD i j, bs2)
    valid bs0 _ = case valid bs0 (undefined :: Integer) of
                  Left err -> Left err
                  Right bs1 ->
                      valid bs1 (undefined :: Integer)
    output (TOD i j) = output i `BS.append` output j

instance InputOutput a => InputOutput [a] where
    input = inputList
    valid bs _ = validList bs (undefined :: a)
    output = outputList

inputList :: InputOutput a => ByteString -> ([a], ByteString)
inputList bs = case BS.head bs of
               0 -> ([], BS.tail bs)
               1 -> case input (BS.tail bs) of
                    (x, bs') ->
                        case inputList bs' of
                        -- XXX This ~ is just for lazy inventory reading;
                        -- use a different type?
                        ~(xs, bs'') -> (x : xs, bs'')
               _ -> error "InputOutput []: Bad value"

validList :: InputOutput a
          => ByteString -> a -> Either (String, ByteString) ByteString
validList bs typeProxy = case BS.uncons bs of
                         Just (0, bs') -> Right bs'
                         Just (1, bs') ->
                             case valid bs' typeProxy of
                             Right bs'' ->
                                 validList bs'' typeProxy
                             Left err -> Left err
                         _ -> Left ("InputOutput []: Bad value", bs)

outputList :: InputOutput a => [a] -> ByteString
outputList [] = BS.singleton 0
outputList (x : xs) = 1 `BS.cons` output x `BS.append` outputList xs

