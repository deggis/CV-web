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
import System.Environment
import System.Exit
import Data.ByteString (ByteString)
import Data.Maybe

import qualified Data.Aeson.Encode as JSON

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Base64 as B64

import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import qualified Crypto.Hash.MD5 as C
import CV.Image
import qualified CV.Transforms as T

import Source
import CVWeb
import Config

data App = App { appHeist    :: HeistState Snap
               , gallery     :: MVar Gallery
               , config      :: Configuration
               , compileLock :: CompileLock }

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) (putStrLn "USAGE: ./cv-web CONF_FILE!" >> exitFailure)

    Right heist <- loadTemplates "web" defaultHeistState
    app         <- newApp (head args) heist
    quickHttpServe $
        route [ ("editor",          editor app)
              , ("upload",          upload app)
              , ("viewerPopup",     viewerPopup app)
              , ("eval",            eval app)
              , ("image/:hash",     image app)
              , ("thumbnail/:hash", thumbnail app) ]
        <|> serveDirectory "web"

newApp :: FilePath -> HeistState Snap -> IO App
newApp confFile appHeist = do
    conf' <- getTemporaryDirectory >>= \fp -> readConfig confFile fp
    case conf' of
        Left (_,msgs) -> do
            putStrLn msgs
            exitFailure
        Right config@Configuration{..} -> do
            print config
            createDirectoryIfMissing False workDir
            gallery <- newMVar M.empty

            -- Let's add previously added uploads also.
            uploads <- getDirectoryContents uploadDir
            let fileNames = filter (\fn->length fn > 2) uploads
                paths = map (\f->uploadDir++"/"++f) fileNames
                comb = zip paths fileNames
            mapM_ (uncurry $ addToGallery gallery) comb
            compileLock <- newCompileLock
            return App{..}

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


-- |Handler for receiving sent images.
-- Request's Content-type must be multipart/formdata
upload :: App -> Snap ()
upload app@App{..} = do
    fnames <- handleFileUploads (tempDir++"/") defaultUploadPolicy (\_ -> allowWithMaximumSize 1000000) (uploadHandler app)
    mapM_ (writeText . T.pack)  fnames
  where
    Configuration{..} = config

uploadHandler :: App -> [(PartInfo, Either PolicyViolationException FilePath)] -> Snap [FilePath]
uploadHandler app xs = do
    mapM_ (receiveFile app) xs
    redirect "/editor"

receiveFile :: App -> (PartInfo, Either PolicyViolationException FilePath) -> Snap FilePath
receiveFile App{..} transmission =
    case partFileName . fst $ transmission of
        Just bfn -> do
            let Right loc = snd transmission
            let fn = T.unpack . E.decodeUtf8 $ bfn
            liftIO $ addToGallery gallery loc fn
            return fn
        Nothing -> pass

addToGallery :: MVar Gallery -> FilePath -> FilePath -> IO ()
addToGallery gallery fullPath fileName = do
    i' <- liftIO $ loadImage fullPath
    let i = fromMaybe (error ("File "++fullPath++" did not exist!")) i'
    modifyMVar gallery (\m -> return (M.insert fileName i m, ()))

viewerPopup :: App -> Snap ()
viewerPopup app = do
    Just (b,t) <- renderTemplate
        (appHeist app)
        "viewer"
    modifyResponse (setContentType t)
    writeBuilder b

getSource :: Snap ByteString
getSource = maybe pass return =<< getParam "source"

-- |Handler that evaluates source code, creates the image and
-- returns URL for the image.
-- FIXME: code getting laddery
eval :: App -> Snap ()
eval App{..} = do
    src <- getSource
    let fpart = fileNameFromDigest . C.hash $ src
        Configuration{..} = config
        pth = imF workDir fpart
    exists <- liftIO $ doesFileExist pth
    if exists
        then writeJSON [("status","ok"),("hash",fpart)]
        else do
            let srcPath = srcF workDir fpart
            liftIO $ B.writeFile srcPath src
            -- |Limit compiling not to be done in parallel for now
            -- to avoid linker errors.
            (msgs,compiled) <- liftIO . withLock compileLock $ compile config srcPath
            liftIO $ mapM_ putStrLn msgs
            gal <- liftIO $ readMVar gallery
            case compiled of 
                Just f -> do
                    let result = f $ createLookup gal
                    liftIO $ saveImage pth result
                    let tn_pth = imF workDir (fpart++"_tn")
                        tn = T.scaleToSize T.Linear True (70,70) result
                    liftIO $ saveImage tn_pth tn
                    writeJSON [("status","ok"),("hash",fpart)]
                Nothing ->
                    writeJSON [("status","failure"),("reason",unlines msgs)]

writeJSON :: [(String,String)] -> Snap ()
writeJSON = writeBS . B.concat . BL.toChunks . JSON.encode . M.fromList

-- |Creates a dirty lookup to enable coding without Maybes.
createLookup :: Gallery -> FilePath -> Im
createLookup gal fn =
    case M.lookup fn gal of
        Just i -> i
        Nothing -> let files = show $ M.keys gal
                   in error ("Image "++fn++"not found! Images available: "++files)

-- | Handler for retrieving generated images.
image :: App -> Snap ()
image = image' ""

-- | Handler for retrieving thumbnails of generated images.
thumbnail :: App -> Snap ()
thumbnail = image' "_tn"

image' :: String -> App -> Snap ()
image' postFix App{..} = do
    hash <- maybe pass (return.sanitizeBS) =<< getParam "hash"
    serveFile $ imF workDir (hash++postFix) -- FIXME
  where
    Configuration{..} = config

-- FIXME: hash must be sanitized!
srcF :: FilePath -> String -> FilePath
srcF workDir hash = workDir++"/"++hash++".hs"

-- FIXME: hash must be sanitized!
imF :: FilePath -> String -> FilePath
imF workDir hash = workDir++"/"++hash++".png"

fileNameFromDigest :: ByteString -> FilePath
fileNameFromDigest = sanitizeBS . B64.encode

sanitizeBS :: ByteString -> FilePath
sanitizeBS = BC.unpack . BC.filter (`BC.elem` allowed)
  where
    allowed = B.pack $ [65..90] ++ [97..122] ++ [48..57] ++ [45,95]
