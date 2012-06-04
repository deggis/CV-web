{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Config where

import Data.ConfigFile
import Data.Either.Utils
import Control.Monad.Error
import System.Directory

data Configuration = Configuration { uploadDir :: FilePath
                                   , workDir :: FilePath
                                   , tempDir :: FilePath
                                   , extraPkgConfs :: [FilePath]
                                   , thumbnailSize :: Int
                               }
    deriving(Show)

--readSetup :: FilePath -> IO 
readConfig filePath tempDir = runErrorT $ do
    cp <- join . liftIO . readfile emptyCP $ filePath

    thumbnailSizeStr <- get cp "GENERAL" "thumbnailsize"
    let thumbnailSize = read thumbnailSizeStr :: Int

    uploadDir <- get cp "PATHS" "uploadDir"
    workDir <- get cp "PATHS" "workDir"

    packageConfs <- get cp "PATHS" "packageConf"
    let extraPkgConfs = [packageConfs]

    return $ Configuration{..}
