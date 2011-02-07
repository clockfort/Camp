
module Camp.Messages (
    msg
    ) where

import Camp.Options

import Control.Monad

msg :: GeneralFlags -> Verbosity -> String -> IO ()
msg gf v str = when (gfVerbosity gf >= v) $ putStrLn str

