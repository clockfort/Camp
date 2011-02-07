
module Camp.Command.Init (initialise) where

import Camp.Logging
import Camp.Options
import Camp.Repository

initialise :: GeneralFlags -> [String] -> IO ()
initialise gf [] = withLockedRepoCreate $ \r -> do
                       initialiseRepo r
                       withLog gf r $ \_ -> return ()
initialise _ _ = error "Unknown arguments to initialise"

