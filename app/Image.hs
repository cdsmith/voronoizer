module Image
  ( Grid (..),
    getImage,
    saveImage,
    fromImage,
    toImage,
    generateGrid,
    gridAt,
  )
where

import Codec.Picture
  ( DynamicImage (..),
    Image (..),
    PixelRGB8 (..),
    convertRGB8,
    generateImage,
    pixelAt,
    readImage,
    savePngImage,
  )
import Color (CIELab, colorToPixel, pixelToColor)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as Vector
import Foreign.Storable (Storable (..))

getImage :: FilePath -> IO (Image PixelRGB8)
getImage file = do
  eitherImg <- readImage file
  case eitherImg of
    Left err -> error err
    Right img -> return (convertRGB8 img)

saveImage :: FilePath -> Image PixelRGB8 -> IO ()
saveImage f = savePngImage f . ImageRGB8

data Grid a = Grid
  { gridWidth :: !Int,
    gridHeight :: !Int,
    gridData :: !(Vector a)
  }

generateGrid :: (Storable a) => Int -> Int -> (Int -> Int -> a) -> Grid a
generateGrid w h f =
  Grid w h (Vector.generate (w * h) (uncurry f . indexToCoords))
  where
    indexToCoords i = (i `mod` w, i `div` w)

gridAt :: Storable a => Grid a -> Int -> Int -> a
gridAt (Grid w _ v) x y = v Vector.! (y * w + x)

-- | Convert an image in RGB to a 2D vector of colors
fromImage ::
  (Storable a, Ord a, Floating a) => Image PixelRGB8 -> Grid (CIELab a)
fromImage img = generateGrid (imageWidth img) (imageHeight img) $ \x y ->
  pixelToColor (pixelAt img x y)

toImage ::
  (Storable a, Floating a, RealFrac a) => Grid (CIELab a) -> Image PixelRGB8
toImage grid = generateImage render (gridWidth grid) (gridHeight grid)
  where
    render x y = colorToPixel (gridAt grid x y)
