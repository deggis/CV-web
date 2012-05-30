{-# LANGUAGE CPP #-}

{-|
    Save off signal handlers and such, so that they can be restored following
    the use of the GHC API.  Code shamelessly stolen from Snap.
-}
module ProtectHandlers (protectHandlers) where

import Control.Exception

#ifdef mingw32_HOST_OS

{-
    Win32 Version: Save the console handler using GHC primitives.
-}

import GHC.ConsoleHandler as C


saveHandlers :: IO C.Handler
saveHandlers = C.installHandler Ignore


restoreHandlers :: C.Handler -> IO C.Handler
restoreHandlers = C.installHandler

#else

{-
    UNIX Version: Save signal handlers
-}

import qualified System.Posix.Signals as S


helper :: S.Handler -> S.Signal -> IO S.Handler
helper handler signal = S.installHandler signal handler Nothing


signals :: [S.Signal]
signals = [ S.sigQUIT
          , S.sigINT
          , S.sigHUP
          , S.sigTERM
          ]


saveHandlers :: IO [S.Handler]
saveHandlers = mapM (helper S.Ignore) signals


restoreHandlers :: [S.Handler] -> IO [S.Handler]
restoreHandlers h = sequence $ zipWith helper h signals
#endif

{-|
    Run an IO action with handlers saved and restored.
-}
protectHandlers :: IO a -> IO a
protectHandlers a = bracket saveHandlers restoreHandlers $ const a

