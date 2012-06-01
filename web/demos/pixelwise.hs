import CV.Image
import qualified CV.Pixelwise as P
import CVWeb

image = return $ P.imageFromFunction (500,500)
         (\(i,j)->max (fromIntegral i/500) (fromIntegral j/500))
