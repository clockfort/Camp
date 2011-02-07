
-- We don't know what type CURLcode (for example) is going to be, so we
-- don't know whether or not we need Data.Int and Data.Word:
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Th include currently gives us a deprecated message
{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}

module Camp.Curl (
    URL, mkURL, (</>),
    withGlobalCurl,
    Content(..),
    downloadToTemporaryFile, downloadContentsToTemporaryFile,
    -- XXX Do we need to export anything below here?
    withCurl,
    perform,
    setURL, setRange, unsetRange,
    setSSHPrivateKey, setSSHPublicKey, setVerbosity,
    setWriteFunction, makeWriteFunction,
    setDebugFunction, makeDebugFunction
    ) where

#include <curl/curl.h>

import Camp.Content
import Camp.InRepoFileName (InRepoFileName)
import qualified Camp.InRepoFileName as FN
import Camp.Types

import Control.Exception
import Control.Monad
import Data.Int
import Data.Word
import Foreign
import Foreign.C
import qualified System.FilePath as FP
import System.IO

newtype URL = URL String
    deriving (Eq, Ord, Show)

mkURL :: String -> URL
mkURL = URL

(</>) :: URL -> InRepoFileName -> URL
URL x </> y = URL (x FP.</> FN.toFilePath y)

type CURLcode = #type CURLcode
type CURLoption = #type CURLoption
-- type CurlOff = #type curl_off_t
type CurlInfo = #type curl_infotype

data CurlHandle
newtype Curl = Curl (Ptr CurlHandle)

-- We always use CURL_GLOBAL_ALL rather than trying to predict what
-- we'll want to use
withGlobalCurl :: IO a -> IO a
withGlobalCurl = bracket_ initialise cleanup
    where initialise = do cc <- curl_global_init initialiseAll
                          checkForCurlException cc
          cleanup = curl_global_cleanup

initialiseAll :: CLong
initialiseAll = #const CURL_GLOBAL_ALL

withCurl :: (Curl -> IO a) -> IO a
withCurl = bracket initialise cleanup
    where initialise = do c@(Curl p) <- curl_easy_init
                          when (p == nullPtr) $
                              error "Curl init failed"
                          return c
          cleanup = curl_easy_cleanup

perform :: Curl -> IO ()
perform c = do cc <- curl_easy_perform c
               checkForCurlException cc

setVerbosity :: Curl -> Bool -> IO ()
setVerbosity c verbose = do cc <- curl_easy_setopt_long c opt v
                            checkForCurlException cc
    where opt = #const CURLOPT_VERBOSE
          v = if verbose then 1 else 0

setURL :: Curl -> URL -> IO ()
setURL c (URL url) = do cc <- withCString url $ curl_easy_setopt_ptr c opt
                        checkForCurlException cc
    where opt = #const CURLOPT_URL

setRange :: Curl -> Integer -> Integer -> IO ()
setRange c from to = do cc <- withCString (show from ++ "-" ++ show to) $
                            curl_easy_setopt_ptr c opt
                        checkForCurlException cc
    where opt = #const CURLOPT_RANGE

unsetRange :: Curl -> IO ()
unsetRange c = do cc <- curl_easy_setopt_ptr c opt nullPtr
                  checkForCurlException cc
    where opt = #const CURLOPT_RANGE

setSSHPrivateKey :: Curl -> FilePath -> IO ()
setSSHPrivateKey c fp = do cc <- withCString fp $ curl_easy_setopt_ptr c opt
                           checkForCurlException cc
    where opt = #const CURLOPT_SSH_PRIVATE_KEYFILE

setSSHPublicKey :: Curl -> FilePath -> IO ()
setSSHPublicKey c fp = do cc <- withCString fp $ curl_easy_setopt_ptr c opt
                          checkForCurlException cc
    where opt = #const CURLOPT_SSH_PUBLIC_KEYFILE

setWriteFunction :: Curl -> FunPtr WriteFunction -> IO ()
setWriteFunction c wf =
    do cc <- curl_easy_setopt_funptr c opt wf
       checkForCurlException cc
    where opt = #const CURLOPT_WRITEFUNCTION

setDebugFunction :: Curl -> FunPtr DebugFunction -> IO ()
setDebugFunction c df =
    do cc <- curl_easy_setopt_funptr c opt df
       checkForCurlException cc
    where opt = #const CURLOPT_DEBUGFUNCTION

checkForCurlException :: CURLcode -> IO ()
checkForCurlException cc
    = when (cc /= 0) $
          do err <- curl_easy_strerror cc
             str <- peekCString err
             error ("Curl failed with code " ++ show cc ++ ": " ++ str)

-- CURLcode curl_global_init(long flags );
foreign import ccall unsafe "curl_global_init"
    curl_global_init :: CLong -> IO CURLcode

-- void curl_global_cleanup(void);
foreign import ccall unsafe "curl_global_cleanup"
    curl_global_cleanup :: IO ()

-- CURL *curl_easy_init( );
foreign import ccall unsafe "curl_easy_init"
    curl_easy_init :: IO Curl

-- void curl_easy_cleanup(CURL * handle );
foreign import ccall unsafe "curl_easy_cleanup"
    curl_easy_cleanup :: Curl -> IO ()

-- CURLcode curl_easy_perform(CURL * handle );
foreign import ccall safe "curl_easy_perform"
    curl_easy_perform :: Curl -> IO CURLcode

-- CURLcode curl_easy_setopt(CURL *handle, CURLoption option, parameter);
-- parameter can be:
--     a long
--     a function pointer
--     an object pointer
--     a curl_off_t
foreign import ccall unsafe "curl_easy_setopt_long"
    curl_easy_setopt_long :: Curl -> CURLoption -> CLong -> IO CURLcode
foreign import ccall unsafe "curl_easy_setopt_funptr"
    curl_easy_setopt_funptr :: Curl -> CURLoption -> FunPtr a -> IO CURLcode
foreign import ccall unsafe "curl_easy_setopt_ptr"
    curl_easy_setopt_ptr :: Curl -> CURLoption -> Ptr a -> IO CURLcode
{-
foreign import ccall unsafe "curl_easy_setopt_off"
    curl_easy_setopt_off :: Curl -> CURLoption -> CurlOff -> IO CURLcode
-}

-- CURLOPT_WRITEFUNCTION
-- size_t function( void *ptr, size_t size, size_t nmemb, void *stream)
type WriteFunction = CString -> CSize -> CSize -> Ptr () -> IO CSize

foreign import ccall "wrapper"
    makeWriteFunction :: WriteFunction -> IO (FunPtr WriteFunction)

-- CURLOPT_DEBUGFUNCTION
-- int curl_debug_callback (CURL *, curl_infotype, char *, size_t, void *);
type DebugFunction = Curl -> CurlInfo -> CString -> CSize -> Ptr () -> IO CInt

foreign import ccall "wrapper"
    makeDebugFunction :: DebugFunction -> IO (FunPtr DebugFunction)

-- const char *curl_easy_strerror(CURLcode  errornum );
foreign import ccall unsafe "curl_easy_strerror"
    curl_easy_strerror :: CURLcode -> IO CString

writeToHandle :: Handle -> CString -> CSize -> CSize -> Ptr () -> IO CSize
writeToHandle h str size memb _ = do let len = size * memb
                                     hPutBuf h str (fromIntegral len)
                                     return len

downloadToTemporaryFile :: URL -> IO FilePath
downloadToTemporaryFile url =
    withCurl $ \curl -> do
    bracket (openBinaryTempFile "/tmp"{- XXX -} "camp.tmp")
            (\(_, h) -> hClose h)
            (\(fp, h) ->
                bracket (makeWriteFunction (writeToHandle h))
                        (\wfp -> do setWriteFunction curl nullFunPtr
                                    freeHaskellFunPtr wfp)
                        (\wfp -> do setWriteFunction curl wfp
                                    setURL curl url
                                    perform curl
                                    return fp))

downloadContentsToTemporaryFile :: [Content URL] -> IO FilePath
downloadContentsToTemporaryFile cs =
    withCurl $ \curl -> do
    bracket (openBinaryTempFile "/tmp"{- XXX -} "camp.tmp")
            (\(_, h) -> hClose h)
            (\(fp, h) ->
                bracket (makeWriteFunction (writeToHandle h))
                        (\wfp -> do setWriteFunction curl nullFunPtr
                                    freeHaskellFunPtr wfp)
                        (\wfp -> do setWriteFunction curl wfp
                                    let f (Content url from len) = do
                                            setURL curl url
                                            let to = from + len - 1
                                            setRange curl (toInteger from)
                                                          (toInteger to)
                                            perform curl
                                    mapM_ f $ simplifyContents cs
                                    return fp))

