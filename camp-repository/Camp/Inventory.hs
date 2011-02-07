
-- XXX Work around GHC warning bugs:
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Camp.Inventory (
    Inventory,
    InRepoInventory,
    AbsoluteInventory,
    InRepoInventoryItem,
    AbsoluteInventoryItem,
    InventoryItem(..),
    toAbsoluteInventory,
    compactInventory,
    compactInventoryWithOffset,
    absoluteInventoryToContents,
    inventoryToContentURLs,
    inventoryToContents
    ) where

import Camp.InRepoFileName (InRepoFileName)
import qualified Camp.InRepoFileName as InRepoFileName
import Camp.Network as Network
import Camp.Patch.InputOutput
import Camp.Patch.Name
import Camp.Types
import Camp.Utils

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified System.FilePath as FP

type Inventory a = [InventoryItem a]

-- XXX Pull may benefit from a contexted sequence of these
data InventoryItem a = InventoryItem Name  -- MegaPatch name
                                     a     -- Filename
                                     Bytes -- Offset
                                     Bytes -- Length

-- In an InRepoInventory, filenames are relative to the working directory
type InRepoInventory = [InRepoInventoryItem]
type InRepoInventoryItem = InventoryItem InRepoFileName
-- In an AbsoluteInventory, filenames are absolute FilePath's
type AbsoluteInventory = [AbsoluteInventoryItem]
type AbsoluteInventoryItem = InventoryItem FilePath

instance InputOutput a => InputOutput (InventoryItem a) where
    input bs0 = case stripPrefixBS inventoryItemHeader bs0 of
                Nothing -> error "XXX Can't happen: No InventoryItem header"
                Just bs1 ->
                    case input bs1 of
                    (n, bs2) ->
                        case input bs2 of
                        (fp, bs3) ->
                            case input bs3 of
                            (from, bs4) ->
                                case input bs4 of
                                (len, bs5) ->
                                    (InventoryItem n fp from len, bs5)
    valid bs _ = case stripPrefixBS inventoryItemHeader bs of
                 Nothing -> Left ("No InventoryItem header", bs)
                 Just bs' ->
                     valid bs'   (undefined :: Name)
                     `thenValid` (undefined :: a)
                     `thenValid` (undefined :: Bytes)
                     `thenValid` (undefined :: Bytes)
    output (InventoryItem n fp from len)
        = inventoryItemHeader `BS.append`
          output n `BS.append`
          output fp `BS.append`
          output from `BS.append`
          output len

inventoryItemHeader :: ByteString
inventoryItemHeader = BSC.pack "\nInventoryItem\n"

inventoryToContentURLs :: URL -> InRepoInventory -> [Content URL]
inventoryToContentURLs root is = map (inventoryItemToContentURL root) is

inventoryItemToContentURL :: URL -> InRepoInventoryItem -> Content URL
inventoryItemToContentURL root (InventoryItem _ fn from len)
 = Content (root Network.</> fn) from len

-- XXX Remove or rename me?
inventoryToContents :: FilePath -> InRepoInventory -> [Content FilePath]
inventoryToContents root is = map (inventoryItemToContent root) is

-- XXX Remove or rename me?
inventoryItemToContent :: FilePath -> InRepoInventoryItem -> Content FilePath
inventoryItemToContent root (InventoryItem _ fn from len)
 = Content (root FP.</> InRepoFileName.toFilePath fn) from len

absoluteInventoryToContents :: AbsoluteInventory -> [Content FilePath]
absoluteInventoryToContents is = map absoluteInventoryItemToContent is

absoluteInventoryItemToContent :: AbsoluteInventoryItem -> Content FilePath
absoluteInventoryItemToContent (InventoryItem _ fp from len)
 = Content fp from len

toAbsoluteInventory :: FilePath -> InRepoInventory -> AbsoluteInventory
toAbsoluteInventory fp = map f
    where f (InventoryItem n fn from len)
              = InventoryItem n
                              (fp FP.</> InRepoFileName.toFilePath fn)
                              from
                              len

compactInventory :: file -> Inventory a -> Inventory file
compactInventory f = compactInventoryWithOffset f 0

compactInventoryWithOffset :: file -> Bytes -> Inventory a -> Inventory file
compactInventoryWithOffset f = compact
    where compact !_      [] = []
          compact !offset (InventoryItem n _ _ len : is)
              = InventoryItem n f offset len : compact (offset + len) is

