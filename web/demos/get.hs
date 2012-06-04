{-|
Example of using resources in CV-web.

From: Heikki Salo / heikki.ao.salo@iki.fi

CC0 1.0
-}

import CV.Image
import CVWeb

-- |The `image' function gets an argument
-- `get' of type (FilePath -> Image GrayScale D32)
-- and serves as lookup function to get demo images.
-- FIXME: Server currently responds 500 if
--        file name was faulty.
image get = get "lena.jpg"
