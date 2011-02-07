
module Camp.Command.Add (add) where

import Camp.Logging
import Camp.Options
import Camp.Repository

add :: GeneralFlags -> [String] -> IO ()
add _ [] = error "No arguments to add"
add gf paths = withLockedRepoSearch $ \r ->
                  withLog gf r $ \_ -> do
                      current <- readAdds r
                      writeAdds r (current ++ paths)

