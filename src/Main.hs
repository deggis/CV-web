{-|
    This module contains Snap application
    for CV-web.

    The design has been adapted from cdsmith's
    gloss-web: https://github.com/cdsmith/gloss-web
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Concurrent.MVar
import Snap.Core
import Snap.Util.FileServe
import Snap.Util.FileUploads
import Snap.Http.Server
import Text.Templating.Heist
import Text.XmlHtml

import qualified Data.Map as M

import System.Directory
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import qualified Data.Text as T
import qualified Data.Text.Encoding as E


import CV.Image

import GHC.Paths

import Source
import CVWeb


-- FIXME: M.Map as collection never releases memory!
data App = App { appHeist    :: HeistState Snap
               , tempDir     :: FilePath
               , workDir     :: FilePath
               , gallery     :: MVar Gallery }

newApp :: HeistState Snap -> IO App
newApp appHeist = do
    tempDir <- getTemporaryDirectory
    let workDir = tempDir++"/"++dirName
    let createParents = False in
        createDirectoryIfMissing createParents workDir
    gallery <- newMVar M.empty
    return $ App{..}
  where
    dirName = "cvweb" :: FilePath

main :: IO ()
main = do
    liftIO $ putStrLn libdir
    liftIO $ putStrLn ghc_pkg
    liftIO $ putStrLn ghc
    liftIO $ putStrLn docdir
    Right heist <- loadTemplates "web" defaultHeistState
    app         <- newApp heist
    quickHttpServe $
        route [ ("editor",         editor app)
              , ("upload",         upload app)
              , ("viewerPopup",         viewerPopup app)
              , ("apiDocSpecs",    apiDocSpecs app)
              , ("apiImage/:hash", apiImage app) ]
        <|> serveDirectory "web"

editor :: App -> Snap ()
editor app = do
    Just (b,t) <- renderTemplate
        (addSplices (appHeist app))
        "editor"
    modifyResponse (setContentType t)
    writeBuilder b
  where
    addSplices = bindSplices [
        ("intro", introSplice),
        ("action", actionSplice),
        ("defaults", defaultsSplice),
        ("settings", settingsSplice)
        ]
    introSplice = return [
        TextNode "Define a variable called ",
        Element "code" [] [TextNode "f :: Image GrayScale D32 -> Image GrayScale D32"],
        TextNode " describing your transformation."
        ]
    actionSplice = return [ TextNode "sendImage" ]
    defaultsSplice = return [ Element "script" [("type", "text/javascript")] [
        TextNode "var sourceCookie = 'displaySource';",
        TextNode "var initialSource = 'import CV.Image\\n",
        TextNode "import qualified CV.Transforms as T\\n\\n",
        TextNode "f = T.flip T.Vertical';"
        ]]
    settingsSplice = return [ Element "script" [("type", "text/javascript")] [
        TextNode "var popup = false;"
        ]]


-- | Request's Content-type must be multipart/formdata
upload :: App -> Snap ()
upload app@App{..} = do
    fnames <- handleFileUploads (tempDir++"/") defaultUploadPolicy (\_ -> allowWithMaximumSize 1000000) (uploadHandler app)
    mapM_ (writeText . T.pack)  fnames

uploadHandler :: App -> [(PartInfo, Either PolicyViolationException FilePath)] -> Snap [FilePath]
uploadHandler app xs = do
    j <- mapM (uploadHandler' app) xs
    -- FIXME: save to ioref tms
    redirect "/editor"

uploadHandler' :: App -> (PartInfo, Either PolicyViolationException FilePath) -> Snap FilePath
uploadHandler' App{..} transmission =
    case (partFileName . fst $ transmission) of
        Just bfn -> do
            let Right loc = snd transmission
            let fn = T.unpack . E.decodeUtf8 $ bfn
            Just i <- liftIO $ loadImage loc
            liftIO $ modifyMVar gallery (\m -> return $ (M.insert fn i m, ()))
            return fn
        Nothing -> pass

viewerPopup :: App -> Snap ()
viewerPopup app = do
    Just (b,t) <- renderTemplate
        (appHeist app)
        "viewer"
    modifyResponse (setContentType t)
    writeBuilder b

getSource :: Snap ByteString
getSource = maybe pass return =<< getParam "source"

apiDocSpecs :: App -> Snap ()
apiDocSpecs App{..} = do
    src <- getSource
    let hash = "srz" -- FIXME
        srcPath = srcF workDir hash
    liftIO $ B.writeFile srcPath src
    (msgs,compiled) <- liftIO $ compile srcPath
    liftIO $ mapM_ putStrLn msgs
    gal <- liftIO $ readMVar gallery
    case compiled of 
        Just f -> do
            let result = runReader f gal
                pth = imF workDir hash
            liftIO $ saveImage pth result
            writeText . T.pack $ hash
        Nothing ->
            writeText . T.pack . unlines $ msgs

apiImage :: App -> Snap ()
apiImage App{..} = do
    hashBS <- maybe pass return =<< getParam "hash"
    let hash = T.unpack . E.decodeUtf8 $ hashBS
    serveFile $ imF workDir hash -- FIXME

-- FIXME: hash must be sanitized!
srcF :: FilePath -> String -> FilePath
srcF workDir hash = workDir++"/"++hash++".hs"

-- FIXME: hash must be sanitized!
imF :: FilePath -> String -> FilePath
imF workDir hash = workDir++"/"++hash++".png"
