{-|
Example of ImageMath.min.

From: Heikki Salo / heikki.ao.salo@iki.fi

CC0 1.0
-}

import CV.Image
import qualified CV.ImageMath as M
import CVWeb

image get = get "lena.jpg" `M.min` get "lena-mask.jpg"
