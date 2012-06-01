module CVWeb where

import qualified Data.Map as M
import Control.Monad.Reader
import Data.Maybe
import CV.Image

type Im = Image GrayScale D32
type Func = CVWebMonad Im

type Gallery = M.Map FilePath Im
type CVWebMonad a = Reader Gallery a

use :: FilePath -> CVWebMonad Im
use fp = do
  gallery <- ask
  return . fromJust . M.lookup fp $ gallery
