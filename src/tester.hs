{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Prelude hiding (catch)

import CV.Image
import System.Environment

import Data.IORef
import GHC.Exts (unsafeCoerce#)
import System.Directory
import System.FilePath
import Control.Exception
import Control.Applicative

import qualified GHC        as GHC
import qualified MonadUtils as GHC
import qualified GHC.Paths  as GHC
import qualified Bag        as GHC
import qualified Outputable as GHC
import qualified ErrUtils   as GHC
import qualified HscTypes   as GHC
import qualified DynFlags   as GHC

import ProtectHandlers

main = do
    [target] <- getArgs
    Just i <- loadImage "/tmp/res/monalisa.jpg"
    (msgs,f) <- compile "/space/heatolsa/projects/jyu.cvw3/src/f.hs"
    mapM_ putStrLn msgs
    let res = f <*> Just i
    case res of
        Just x -> saveImage ("/tmp/res/"++target) x
        Nothing -> putStrLn "Hmpf!"

type Func = Image GrayScale D32 -> Image GrayScale D32

type CompileResult = ([String], Maybe Func)

{-|
    Compile the module in the given file name, and return the named value
    with the given type.  If the type passed in doesn't match the way the
    result is used, the server process will likely segfault.
-}
compile :: FilePath -> IO CompileResult
compile fn = doWithErrors $ do
    dflags <- GHC.getSessionDynFlags
    let dflags1 = dflags {
        GHC.ghcMode = GHC.CompManager,
        GHC.ghcLink = GHC.LinkInMemory,
        GHC.hscTarget = GHC.HscInterpreted
--      GHC.safeHaskell = GHC.Sf_Safe,
--      GHC.packageFlags = [GHC.TrustPackage  "CV",
--                          GHC.TrustPackage "base"]
        }
    let dflags2 = GHC.xopt_unset dflags1 GHC.Opt_MonomorphismRestriction
    let dflags3 = GHC.xopt_set   dflags2 GHC.Opt_MonoLocalBinds
--    let dflags4 = GHC.dopt_set   dflags3 GHC.Opt_PackageTrust
    GHC.setSessionDynFlags dflags3
    target <- GHC.guessTarget fn Nothing
    GHC.setTargets [target]
    r <- fmap GHC.succeeded (GHC.load GHC.LoadAllTargets)
    case r of
        True -> do
            mods <- GHC.getModuleGraph
            let mainMod = GHC.ms_mod (head mods)
            GHC.setContext [ GHC.IIModule mainMod ]
            v <- GHC.compileExpr expr
            return (Just (unsafeCoerce# v))
        False -> return Nothing
  where
    expr = "f :: Image GrayScale D32 -> Image GrayScale D32"

{-|
    Runs an action in the 'Ghc' monad, and automatically collects error
    messages.  There are multiple ways error messages get reported, and
    it's a bit of tricky trial-and-error to handle them all uniformly, so
    this function abstracts that.
-}
doWithErrors :: GHC.Ghc (Maybe Func) -> IO CompileResult
doWithErrors action = do
    codeErrors <- newIORef []
    protectHandlers $ catch (wrapper codeErrors) $ \ (e :: SomeException) -> do
        errs <- readIORef codeErrors
        return (errs, Nothing)
  where
    wrapper codeErrors = fixupErrors codeErrors =<< do
        GHC.defaultErrorHandler (logAction codeErrors)
            $ GHC.runGhc (Just GHC.libdir)
            $ GHC.handleSourceError (handle codeErrors)
            $ do
                dflags <- GHC.getSessionDynFlags
                GHC.setSessionDynFlags dflags {
                    GHC.log_action = logAction codeErrors
                    }
                action
    logAction errs _ span style msg =
        let niceError = GHC.showSDoc
                $ GHC.withPprStyle style $ GHC.mkLocMessage span msg
        in  writeErr errs niceError
    writeErr ref err = modifyIORef ref (++ [ err ])
    handle ref se = do
        let errs    = GHC.bagToList (GHC.srcErrorMessages se)
            cleaned = map (GHC.showSDoc . GHC.errMsgShortDoc) errs
        GHC.liftIO $ modifyIORef ref (++ cleaned)
        return Nothing
    fixupErrors errs (Just x) = fmap (, Just x)  (readIORef errs)
    fixupErrors errs Nothing  = fmap (, Nothing) (readIORef errs)


qualifiedImportDecl m a = (GHC.simpleImportDecl (GHC.mkModuleName m)) {
    GHC.ideclQualified = True,
    GHC.ideclAs = Just (GHC.mkModuleName a)
    }
