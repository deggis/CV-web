module Heh.CVWeb where

import qualified Data.Map as M
import Control.Monad.Reader
import Data.Maybe
import CV.Image
import Debug.Trace

type Im = Image GrayScale D32
type Func = CVWebMonad Im

type Gallery = M.Map FilePath Im
type CVWebMonad a = Reader Gallery a

use :: FilePath -> CVWebMonad Im
use fp = do
  gallery <- ask
  return . trace "Hi, I'm global!" . fromJust . M.lookup fp $ gallery
