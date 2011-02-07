
module Camp.Command.Interactive (interactive) where

import Camp.Patch.Commute
import Camp.Patch.RevSequence
import Camp.Patch.Sequence

import Control.Exception
import System.Exit
import System.IO

interactive :: Commute p p
            => (forall x y . p x y -> String) -> Seq p from to
            -> IO (Then (Seq p) (Seq p) from to)
interactive printer ps = do
    stdinBuf  <- hGetBuffering stdin
    stdoutBuf <- hGetBuffering stdout
    do hSetBuffering stdin  NoBuffering
       hSetBuffering stdout NoBuffering
       select printer NilRevSeq NilRevSeq ps
     `finally` do hSetBuffering stdin  stdinBuf
                  hSetBuffering stdout stdoutBuf

select :: Commute p p
       => (forall c1 c2 . p c1 c2 -> String)
       -> RevSeq p from x -- chosen, in reverse order
       -> RevSeq p x y    -- rejected
       -> Seq p y to      -- undecided
       -> IO (Then (Seq p) (Seq p) from to)
select _ chosen rejected Nil
    = return (toSeq chosen `Then` toSeq rejected)
select printer chosen rejected (p `Cons` ps)
    = do putStrLn $ printer p
         case commute (rejected `Then` p) of
             Nothing ->
                 do putStrLn $ yellow "Skipping due to dependencies"
                    select printer chosen (rejected `Snoc` p) ps
             Just (p' `Then` rejected') ->
                 do putStr $ green "Want this patch? "
                    putStr "[ynq] "
                    c <- getChar
                    putStrLn ""
                    case c of
                        'y' -> select printer (chosen `Snoc` p') rejected' ps
                        'n' -> select printer chosen (rejected `Snoc` p) ps
                        'q' -> exitWith ExitSuccess
                        _   -> select printer chosen rejected (p `Cons` ps)

-- XXX For now we just assume that ANSI escape sequences will work
yellow :: String -> String
yellow xs = "\ESC[0;33m" ++ xs ++ "\ESC[0m"

green :: String -> String
green xs = "\ESC[0;32m" ++ xs ++ "\ESC[0m"

