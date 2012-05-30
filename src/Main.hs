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
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import Text.Templating.Heist
import Text.XmlHtml

import Data.ByteString (ByteString)
import qualified Data.ByteString        as B

import qualified Data.Text              as T

import CV.Image

import Source

data App = App { appHeist    :: HeistState Snap
               , sourceImage :: Image GrayScale D32 }

newApp :: HeistState Snap -> IO App
newApp heist = do
    Just sourceImage <- loadImage "/tmp/reckon/source.jpg" --FIXME: assumed to exist!
    return (App heist sourceImage)

main :: IO ()
main = do
    Right heist <- loadTemplates "web" defaultHeistState
    app         <- newApp heist
    quickHttpServe $
        route [ ("editor",         editor app)
              , ("viewer",         viewer app)
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
        ("defaults", defaultsSplice)
        ]
    introSplice = return [
        TextNode "Define a variable called ",
        Element "code" [] [TextNode "picture"],
        TextNode " describing your picture."
        ]
    actionSplice = return [ TextNode "sendImage" ]
    defaultsSplice = return [ Element "script" [("type", "text/javascript")] [
        TextNode "var sourceCookie = 'displaySource';",
        TextNode "var initialSource = 'import CV.Image\\n",
        TextNode "import qualified CV.Transforms as T\\n\\n",
        TextNode "f = T.flip T.Vertical';"
        ]]


viewer :: App -> Snap ()
viewer app = do
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
    liftIO $ B.writeFile (srcF hash) src
    (msgs,f) <- liftIO $ compile (srcF hash)
    let res = f <*> Just sourceImage
        pth = imF hash
    case res of
        Just x -> do
            liftIO $ saveImage pth x
            writeText . T.pack $ hash
        Nothing ->
            writeText . T.pack . unlines $ msgs

apiImage :: App -> Snap ()
apiImage App{..} = do
    serveFile $ imF "srz" -- FIXME

srcF hash = "/tmp/reckon/"++hash++".hs"
imF hash = "/tmp/reckon/"++hash++".png"
