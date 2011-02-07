
module Camp.Patch.Pretty (Ppr(..), pprint, pprintShow,
                          module Text.PrettyPrint, integral) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Int
import Text.PrettyPrint

pprint :: Ppr a => a -> String
pprint = render . ppr

pprintShow :: Ppr a => a -> String
pprintShow = render . pprShow

class Ppr a where
    ppr :: a -> Doc
    pprAtomic :: a -> Doc
    pprAtomic = parens . ppr

    pprShow :: a -> Doc
    pprShowAtomic :: a -> Doc
    pprShowAtomic = parens . pprShow

instance Ppr a => Ppr [a] where
    ppr xs = brackets $ vcat $ punctuate comma $ map ppr xs
    pprAtomic = ppr
    pprShow xs = brackets $ vcat $ punctuate comma $ map pprShow xs
    pprShowAtomic = pprShow

instance Ppr a => Ppr (Set a) where
    ppr s = braces $ vcat $ punctuate comma $ map ppr $ Set.toList s
    pprAtomic = ppr
    pprShow s = text "Set.fromList" <+> ppr (Set.toList s)

instance Ppr ByteString where
    ppr bs = text $ BSC.unpack bs
    pprAtomic = ppr
    pprShow bs = text "BSC.pack" <+> text (show (BSC.unpack bs))

instance Ppr Integer where
    ppr = integer
    pprAtomic = ppr
    pprShow = ppr
    pprShowAtomic = ppr

instance Ppr Int64 where
    ppr = integer . fromIntegral
    pprAtomic = ppr
    pprShow = ppr
    pprShowAtomic = ppr

integral :: Integral a => a -> Doc
integral = integer . fromIntegral

