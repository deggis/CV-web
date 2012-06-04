module CVWeb where

import qualified Data.Map as M
import CV.Image

type Im = Image GrayScale D32
type Func = (FilePath -> Im) -> Im

type Gallery = M.Map FilePath Im
