
module Main (main) where

import Camp.Command.Add
import Camp.Command.Get
import Camp.Command.Init
import Camp.Command.Inventory
import Camp.Command.Pull
import Camp.Command.Record
import Camp.Command.Show
import Camp.Die
import Camp.Network
import Camp.Options

import Prelude hiding (catch)
import Control.Exception
import System.Environment
import System.Exit

main :: IO ()
main = doIt `catches`
       [Handler $ \e -> throw (e :: ExitCode),
        Handler $ \e -> do
                  die ("Panic! Got an unhandled exception!\n"
                    ++ "Exception details:\n"
                    ++ show (e :: SomeException))]

doIt :: IO ()
doIt = do args <- getArgs
          case args of
              [] -> usageInfo
              _ -> case parseGeneralFlags args of
                   Right (gf, cmd : args') ->
                       withGlobalCurl $
                       case cmd of
                       "add"       -> add        gf args'
                       "get"       -> get        gf args'
                       "init"      -> initialise gf args'
                       "inventory" -> inventory  gf args'
                       "pull"      -> pull       gf args'
                       "record"    -> record     gf args'
                       -- XXX Hack: Handle prefixes properly:
                       "rec"       -> record     gf args'
                       "show"      -> showC      gf args'
                       _ -> die ("Unrecognised command: " ++ show cmd)
                   _ ->
                       do progName <- getProgName
                          die ("Couldn't find a command. Run\n"
                             ++ "    " ++ progName ++ " help\n"
                             ++ "for usage information")

-- XXX When we have a help command, this will be part of it
usageInfo :: IO ()
usageInfo = mapM_ putStrLn [
    "",
    "This is Camp.",
    "",
    "WARNING:",
    "This is not yet suitable for use yet! Do not trust your data to it!",
    "",
    "Not all the below exist yet. Some extra commands may exist.",
    "",
    "Help",
    "    help    Use \"help cmd\" for help on the cmd command",
    "",
    "Repository admin:",
    "    initialise  optimise  check  repair  getpref  setpref",
    "",
    "Inter-repo commands:",
    "    get  put  pull  push  send",
    "",
    "Commands that affect pending changes:",
    "    add  remove  move  revert  unrevert  mark-conflicts",
    "",
    "Commands that affect recorded patches:",
    "    record  unrecord  amend-record  tag  obliterate  invert",
    "",
    "Viewing the repo state:",
    "    whatsnew  annotate  inventory  show",
    ""]


