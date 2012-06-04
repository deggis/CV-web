{-|
Examples on miscellaneous transformations on lena.jpg.

From: Heikki Salo / heikki.ao.salo@iki.fi

CC0 1.0
-}

import CV.Image
import qualified CV.ImageMath as M
import qualified CV.Pixelwise as P
import qualified CV.Filters as F
import qualified CV.Transforms as T
import qualified CV.ColourUtils as CU
import CVWeb

fade = P.imageFromFunction (500,500) f
f (i,j) = max (fromIntegral i/500) (fromIntegral j/500)

image get =
    let lena = get "lena.jpg"
    in montage (3,3) 16 . map smaller $
        [lena
        ,T.rotate 20 lena
        ,F.blur 3 lena

        ,fade
        ,lena `M.add` fade
        ,lena `M.sub` fade

        ,CU.balance (0.1,0.2) lena
        ,lena `M.min` get "lena-mask.jpg"
        ,CU.balance (0.5,1.0) lena ]

smaller = T.scaleToSize T.Linear True (150,150)
