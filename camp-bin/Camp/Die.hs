
module Camp.Die (die) where

import System.Environment
import System.Exit
import System.IO

die :: String -> IO a
die err = do progName <- getProgName
             hPutStrLn stderr (progName ++ ": " ++ err)
             exitFailure

