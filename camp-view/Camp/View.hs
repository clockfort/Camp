
{-
XXX This program is just a quick hack. It can probably be written much
more efficiently, and much more nicely
-}

module Main (main) where

import Camp.Patch.Name
import Camp.Repository

import Camp.View.Deps
import qualified Camp.View.Draw.Text as Text
import qualified Camp.View.Draw.PIL  as PIL
import Camp.View.Types

import System.Environment

main :: IO ()
main = do args <- getArgs
          case args of
              []         -> doit PIL.draw
              ["--pil"]  -> doit PIL.draw
              ["--text"] -> doit Text.draw
              _          -> error "XXX Bad args"

doit :: (Mapping Name DepInfo -> IO ()) -> IO ()
doit drawFun
 = do repo <- getRepo
      inventory <- readInventory repo
      patches <- readMegaPatches repo inventory
      drawFun $ findMarkedDeps patches

