
module Camp.Patch.Apply (Apply(..), flush, applyFully, ApplyState) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Camp.InRepoFileName as InRepoFileName
import Camp.Types

-- While we're applying things, we don't write to
-- disk eagerly as the next thing may want to change the
-- same file
type ApplyState = Maybe (InRepoFileName, -- Filename we are dealing with
                         Bytes,      -- Number of bytes in the first ByteString
                         ByteString, -- These ByteStrings concatenated are
                         ByteString) -- the current required file content

class Apply p where
    apply :: ApplyState -> p from to -> IO ApplyState

flush :: ApplyState -> IO ()
flush Nothing = return ()
flush (Just (fn, _, start, finish))
    = do let fp = InRepoFileName.toFilePath fn
         BS.writeFile fp (start `BS.append` finish)

-- XXX Should we rename this to apply, and apply to someting else?
applyFully :: Apply p => p from to -> IO ()
applyFully p = do m <- apply Nothing p
                  flush m

