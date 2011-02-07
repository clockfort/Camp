
module Camp.Options (
    GeneralFlags(..), parseGeneralFlags,
    RecordFlags(..), parseRecordFlags,
    Verbosity(..) -- XXX Should this be in another module?
    ) where

import System.Console.GetOpt

type ChangeFlags a = (GeneralFlags, a) -> Either String (GeneralFlags, a)

-- XXX Should we put the various options in their own modules?
-- Or in the command modules?

data Verbosity = Silent
               | Normal
               | Verbose
    deriving (Eq, Ord)

data GeneralFlags = GeneralFlags {
                        gfVerbosity :: Verbosity,
                        gfLog       :: Bool
                    }

generalOpts :: [OptDescr (ChangeFlags a)]
generalOpts = [
               Option ['v'] ["verbose"] (OptArg setGfVerbosity "Verbosity") "verbosity",
               Option ['l'] ["log"]     (NoArg (setGfLog True))             "logging",
               Option []    ["no-log"]  (NoArg (setGfLog False))            "no logging"
              ]

setGfLog :: Bool -> ChangeFlags a
setGfLog b (gf, x) = Right (gf { gfLog = b }, x)

setGfVerbosity :: Maybe String -> ChangeFlags a
setGfVerbosity Nothing    (gf, x) = Right (gf { gfVerbosity = Verbose }, x)
setGfVerbosity (Just "0") (gf, x) = Right (gf { gfVerbosity = Silent  }, x)
setGfVerbosity (Just "1") (gf, x) = Right (gf { gfVerbosity = Normal  }, x)
setGfVerbosity (Just "2") (gf, x) = Right (gf { gfVerbosity = Verbose }, x)
setGfVerbosity (Just s)   _  = Left ("Bad verbosity: " ++ show s)

defaultGeneralFlags :: GeneralFlags
defaultGeneralFlags = GeneralFlags {
                          gfVerbosity = Normal,
                          gfLog = False
                      }

parseGeneralFlags :: [String] -> Either [String] (GeneralFlags, [String])
parseGeneralFlags args = case getOpt RequireOrder generalOpts args of
                         (fs, afterOpts, []) ->
                             case apply fs (defaultGeneralFlags, ()) of
                             Left err -> Left [err]
                             Right (flags, ()) -> Right (flags, afterOpts)
                         (_, _, errs) ->
                             Left errs
    where apply [] flags = Right flags
          apply (f:fs) flags = case f flags of
                               Left err -> Left err
                               Right flags' -> apply fs flags'

----------------------------------------------------------------------

type ChangeRecordFlags = ChangeFlags RecordFlags
data RecordFlags = RecordFlags {
                       rfAll     :: Bool,
                       rfMessage :: Maybe String
                   }

recordOpts :: [OptDescr ChangeRecordFlags]
recordOpts = [
               Option ['a'] ["all"]     (NoArg (setRfAll True))         "all",
               Option ['m'] ["message"] (ReqArg setRfMessage "Message") "message"
              ]

setRfAll :: Bool -> ChangeRecordFlags
setRfAll b (gf, rf) = Right (gf, rf { rfAll = b })

-- XXX Check for \n's? Or unlines it and use them for the long message?
setRfMessage :: String -> ChangeRecordFlags
setRfMessage msg (gf, rf) = Right (gf, rf { rfMessage = Just msg })

defaultRecordFlags :: RecordFlags
defaultRecordFlags = RecordFlags {
                         rfAll = False,
                         rfMessage = Nothing
                     }

parseRecordFlags :: [String] -> GeneralFlags
                 -> Either [String] (GeneralFlags, RecordFlags, [String])
parseRecordFlags args gf = case getOpt RequireOrder (recordOpts ++ generalOpts) args of
                           (fs, afterOpts, []) ->
                               case apply fs (gf, defaultRecordFlags) of
                               Left err -> Left [err]
                               Right (gf', rf) -> Right (gf', rf, afterOpts)
                           (_, _, errs) ->
                               Left errs
    where apply [] flags = Right flags
          apply (f:fs) flags = case f flags of
                               Left err -> Left err
                               Right flags' -> apply fs flags'

