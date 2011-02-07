
module Camp.Content (Content(..), simplifyContents) where

import Camp.Types

data Content loc = Content loc Bytes{- from -} Bytes{- length -}
    deriving Show

-- When we have a list of contents like
--     myfile 100 3
--     myfile 103 6
-- we don't want to download bytes 100-102 and 103-108 separately, so
-- this function will merge them into a single content of 100-108.
simplifyContents :: Eq loc => [Content loc] -> [Content loc]
simplifyContents [] = []
simplifyContents (Content loc from len : is)
 = let f cur [] = [Content loc from (cur - from)]
       f cur is'@(Content loc' from' len' : is'')
        | (loc == loc') && (cur == from') = f (cur + len') is''
        | otherwise = Content loc from (cur - from) : simplifyContents is'
   in f (from + len) is

