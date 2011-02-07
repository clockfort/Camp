
module Camp.Patch.Name (
    Named(..), Named1(..),
    Name(..), Sign(..), SubName(..), inverseName, inverseSubName,
    parseName,
    ) where

import Camp.Patch.Pretty
import Camp.Patch.InputOutput

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.ByteString.Lazy (ByteString)
import Data.Char

class Ord n => Named p n | p -> n where
    name :: p from to -> n

class Ord n => Named1 p n | p -> n where
    name1 :: p from -> n

data Name = Name Sign ByteString
    deriving (Eq, Ord)

parseName :: String -> Maybe Name
parseName bs = case bs of
               'P':'-':bs' -> mkName Positive bs'
               'N':'-':bs' -> mkName Negative bs'
               _           -> Nothing
    where mkName sign bs' = if null bs' || not (all good bs')
                            then Nothing
                            else Just (Name sign (BSC.pack bs'))
          good c = (c >= '0' && c <= '9')
                || (c >= 'a' && c <= 'z')
                || (c >= 'A' && c <= 'Z')
                || (c == '-' || c == '.')

instance Ppr Name where
    ppr (Name sign n) = ppr sign <> char '-' <> text (BSC.unpack n)
    pprAtomic = ppr
    pprShow (Name sign n) = text "Name"
                        <+> pprShowAtomic sign
                        <+> pprShowAtomic n

instance InputOutput Name where
    input bs = case input bs of
               (sign, bs') ->
                   case input bs' of
                   (n, bs'') -> (Name sign n, bs'')
    valid bs _ = case valid bs (undefined :: Sign) of
                 Left err -> Left err
                 Right bs' ->
                     case valid bs' (undefined :: ByteString) of
                     Left err -> Left err
                     Right bs'' ->
                         case input bs' of
                         (n, _) ->
                             if BS.null n || not (BS.all goodChar n)
                             then Left ("InputOutput Name Bad value", bs'')
                             else Right bs''
        where goodChar w = ((w >= ord' '0') && (w <= ord' '9'))
                        || ((w >= ord' 'a') && (w <= ord' 'z'))
                        || ((w >= ord' 'A') && (w <= ord' 'Z'))
                        ||  (w == ord' '.')
                        ||  (w == ord' '-')
              ord' = fromIntegral . ord
    output (Name sign n) = output sign `BS.append` output n

data SubName = SubName Name Integer
    deriving (Eq, Ord)

instance Ppr SubName where
    ppr (SubName n s) = ppr n <> colon <> integer s
    pprAtomic = ppr
    pprShow (SubName n s) = text "SubName"
                        <+> pprShowAtomic n
                        <+> pprShowAtomic s

instance InputOutput SubName where
    input bs = case input bs of
               (n, bs') ->
                   case input bs' of
                   (s, bs'') ->
                       (SubName n s, bs'')
    valid bs _ = case valid bs (undefined :: Name) of
                 Left err -> Left err
                 Right bs' ->
                     valid bs' (undefined :: Integer)
    output (SubName n s) = output n `BS.append` output s

data Sign = Positive | Negative
    deriving (Eq, Ord)

instance InputOutput Sign where
    input bs = case BS.head bs of
               0 -> (Positive, BS.tail bs)
               1 -> (Negative, BS.tail bs)
               _ -> error "InputOutput Sign: Bad value"
    valid bs _ = case BS.uncons bs of
               Just (0, bs') -> Right bs'
               Just (1, bs') -> Right bs'
               _ -> Left ("InputOutput Sign: Bad value", bs)
    output Positive = BS.singleton 0
    output Negative = BS.singleton 1

instance Ppr Sign where
    ppr Positive = char 'P'
    ppr Negative = char 'N'
    pprAtomic = ppr
    pprShow Positive = text "Positive"
    pprShow Negative = text "Negative"
    pprShowAtomic = pprShow

instance Read Sign where
    readsPrec _ ('P' : xs) = [(Positive, xs)]
    readsPrec _ ('N' : xs) = [(Negative, xs)]
    readsPrec _ _ = []

inverseName :: Name -> Name
inverseName (Name Positive n) = Name Negative n
inverseName (Name Negative n) = Name Positive n

inverseSubName :: SubName -> SubName
inverseSubName (SubName n i) = SubName (inverseName n) i

