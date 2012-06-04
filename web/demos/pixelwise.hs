{-|
Example of Pixelwise.imageFromFunction.

From: Heikki Salo / heikki.ao.salo@iki.fi

CC0 1.0
-}

import CV.Image
import qualified CV.Pixelwise as P
import CVWeb

image get = P.imageFromFunction (500,500) f

f (i,j) = max (fromIntegral i/500) (fromIntegral j/500)
