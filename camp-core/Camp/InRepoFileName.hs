
-- XXX Rename to RootedFileName?

module Camp.InRepoFileName
    (InRepoFileName, fromString, fromByteString, toFilePath, (</>))
    where

import Camp.Patch.InputOutput
import Camp.Patch.Pretty
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified System.FilePath as FP

-- XXX Should we use [Word8] instead of FilePath?
newtype InRepoFileName = InRepoFileName FilePath

instance Eq InRepoFileName where
    InRepoFileName fn1 == InRepoFileName fn2 = fn1 == fn2

instance Show InRepoFileName where
    showsPrec _ (InRepoFileName fn) = showString fn

instance Ppr InRepoFileName where
    ppr (InRepoFileName fp) = text fp
    pprAtomic = ppr
    pprShow (InRepoFileName fp) = text "InRepoFileName.fromString" <+> text (show fp)
    pprShowAtomic = pprShow

-- XXX Need to do the right checks in the right method (input vs valid)
instance InputOutput InRepoFileName where
    input bs0 = case input bs0 of
                (fnBS, bs1) -> (fromByteString fnBS, bs1)
    output fn = output $ BSC.pack $ toFilePath fn
    valid bs0 _ = valid bs0 (undefined :: ByteString)

-- XXX We should check that we haven't passed in "../foo", "/foo" etc,
-- check for case-sensitive filenames, filenames like "NUL", think about
-- files systems that use unicode encodings, etc. See eg:
-- * http://en.wikipedia.org/wiki/Filename
-- * http://en.wikipedia.org/wiki/Comparison_of_file_systems

fromString :: String -> InRepoFileName
fromString fp = let fp' = FP.normalise fp
                in InRepoFileName fp'

fromByteString :: ByteString -> InRepoFileName
fromByteString = fromString . BSC.unpack

toFilePath :: InRepoFileName -> FilePath
toFilePath (InRepoFileName fp) = fp

(</>) :: InRepoFileName -> InRepoFileName -> InRepoFileName
InRepoFileName x </> InRepoFileName y = InRepoFileName (x FP.</> y)

