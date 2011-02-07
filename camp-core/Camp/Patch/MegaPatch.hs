
-- XXX Work around GHC warning bugs:
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Camp.Patch.MegaPatch (
    MetaInfo(..), MegaPatch(..), commuteToPrefix
    )
    where

import Camp.Patch.Apply
import Camp.Patch.Catch
import Camp.Patch.Commute
import Camp.Patch.Equality
import Camp.Patch.InputOutput
import Camp.Patch.Invert
import Camp.Patch.Name
import Camp.Patch.Pretty
import Camp.Patch.Sequence
import Camp.Utils

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import System.Time
import Unsafe.Coerce

data MegaPatch from to where
    MegaPatch :: Name -> MetaInfo -> Seq Catch from to -> MegaPatch from to

instance InputOutput (MegaPatch from to) where
    input bs0 = case stripPrefixBS megaPatchHeader bs0 of
                Nothing -> error "XXX Can't happen: No MegaPatch header"
                Just bs1 ->
                    case input bs1 of
                    (n, bs2) ->
                        case input bs2 of
                        (mi, bs3) ->
                            case input bs3 of
                            (cs, bs4) ->
                                (MegaPatch n mi cs, bs4)
    valid bs _ = case stripPrefixBS megaPatchHeader bs of
                 Nothing -> Left ("No MegaPatch header", bs)
                 Just bs' ->
                     valid bs'   (undefined :: Name)
                     `thenValid` (undefined :: MetaInfo)
                     `thenValid` (undefined :: Seq Catch from to)
    output (MegaPatch n mi cs) = megaPatchHeader `BS.append`
                                 output n `BS.append`
                                 output mi `BS.append`
                                 output cs

megaPatchHeader :: ByteString
megaPatchHeader = BSC.pack "\nMegaPatch\n"

instance InputOutput2 MegaPatch where
    input2 = input
    output2 = output
    valid2 = valid

instance Ppr (MegaPatch from to) where
    ppr (MegaPatch n mi p) = text "MegaPatch" <+> pprAtomic n
                          $$ nest 4 (ppr mi)
                          $$ nest 4 (pprAtomic p)
    pprShow (MegaPatch n mi p) = text "MegaPatch" <+> pprShowAtomic n
                              $$ nest 4 (pprShow mi)
                              $$ nest 4 (pprShowAtomic p)

instance Named MegaPatch Name where
    name (MegaPatch n _ _) = n

instance Equality MegaPatch where
    -- XXX Should actually check the MegaPatches are equal!
    isEqual _ _ = unsafeCoerce IsEqual

instance Commute MegaPatch MegaPatch where
    commute (MegaPatch np mip p `Then` MegaPatch nq miq q)
        = do (q' `Then` p') <- commute (p `Then` q)
             return (MegaPatch nq miq q' `Then` MegaPatch np mip p')

instance Invert MegaPatch where
    invert (MegaPatch n mi p) = MegaPatch (inverseName n) mi (invert p)

instance Apply MegaPatch where
    apply m (MegaPatch _ _ p) = apply m p



data MetaInfo = MetaInfo ByteString -- Short description
                         ByteString -- Long description
                         ByteString -- Author
                         ClockTime  -- Date of recording. XXX from old-time

instance Ppr MetaInfo where
    ppr (MetaInfo short _long author date)
        = (text $ show date) <> text "  " <> (text $ BSC.unpack author)
       $$ text "  * " <> (text $ BSC.unpack short)
    pprShow (MetaInfo short long author date)
        = text "MetaInfo"
       $$ nest 4 (pprShowAtomic short)
       $$ nest 4 (pprShowAtomic long)
       $$ nest 4 (pprShowAtomic author)
       $$ nest 4 (parens (text "read" <+> text (show (show date))))

instance InputOutput MetaInfo where
    input bs0 = case input bs0 of
                (short, bs1) ->
                    case input bs1 of
                    (long, bs2) ->
                        case input bs2 of
                        (author, bs3) ->
                            case input bs3 of
                            (date, bs4) ->
                                (MetaInfo short long author date, bs4)
    valid bs0 _ = case valid bs0 (undefined :: ByteString) of
                  Left err -> Left err
                  Right bs1 ->
                      case valid bs1 (undefined :: ByteString) of
                      Left err -> Left err
                      Right bs2 ->
                          case valid bs2 (undefined :: ByteString) of
                          Left err -> Left err
                          Right bs3 -> valid bs3 (undefined :: ClockTime)
    output (MetaInfo short long author date)
        = output short `BS.append` output long `BS.append`
          output author `BS.append` output date

