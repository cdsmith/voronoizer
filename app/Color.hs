module Color where

import Codec.Picture (PixelRGB8 (..))
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))

data CIELab = CIELab !Float !Float !Float
  deriving (Eq, Ord, Show)

instance Storable CIELab where
  sizeOf _ = 3 * sizeOf (undefined :: Float)
  alignment _ = alignment (undefined :: Float)

  peek ptr = do
    l <- peek (castPtr ptr)
    a <- peekByteOff ptr (sizeOf (undefined :: Float))
    b <- peekByteOff ptr (2 * sizeOf (undefined :: Float))
    return $ CIELab l a b

  poke ptr (CIELab l a b) = do
    poke (castPtr ptr) l
    pokeByteOff ptr (sizeOf (undefined :: Float)) a
    pokeByteOff ptr (2 * sizeOf (undefined :: Float)) b

-- | Convert a JuicyPixel to a CIELab
--
-- Math from https://github.com/antimatter15/rgb-lab/blob/master/color.js
pixelToColor :: PixelRGB8 -> CIELab
pixelToColor (PixelRGB8 r g b) = CIELab ll aa bb
  where
    r' = fromIntegral r / 255
    g' = fromIntegral g / 255
    b' = fromIntegral b / 255

    r'' = if r' > 0.04045 then ((r' + 0.055) / 1.055) ** 2.4 else r' / 12.92
    g'' = if g' > 0.04045 then ((g' + 0.055) / 1.055) ** 2.4 else g' / 12.92
    b'' = if b' > 0.04045 then ((b' + 0.055) / 1.055) ** 2.4 else b' / 12.92

    x = (0.4124 * r'' + 0.3576 * g'' + 0.1805 * b'') / 0.95047
    y = (0.2126 * r'' + 0.7152 * g'' + 0.0722 * b'') / 1.00000
    z = (0.0193 * r'' + 0.1192 * g'' + 0.9505 * b'') / 1.08883

    x' = if x > 0.008856 then x ** (1 / 3) else 7.787 * x + 16 / 116
    y' = if y > 0.008856 then y ** (1 / 3) else 7.787 * y + 16 / 116
    z' = if z > 0.008856 then z ** (1 / 3) else 7.787 * z + 16 / 116

    ll = 116 * y' - 16
    aa = 500 * (x' - y')
    bb = 200 * (y' - z')

-- | Convert a JuicyPixel to a Color
--
-- Math from https://github.com/antimatter15/rgb-lab/blob/master/color.js
colorToPixel :: CIELab -> PixelRGB8
colorToPixel (CIELab cieL cieA cieB) = PixelRGB8 r'' g'' b''
  where
    x = cieA / 500 + y
    y = (cieL + 16) / 116
    z = y - cieB / 200

    x' = 0.95047 * if x * x * x > 0.008856 then x * x * x else (x - 16 / 116) / 7.787
    y' = 1.00000 * if y * y * y > 0.008856 then y * y * y else (y - 16 / 116) / 7.787
    z' = 1.08883 * if z * z * z > 0.008856 then z * z * z else (z - 16 / 116) / 7.787

    r = x' * 3.2406 + y' * (-1.5372) + z' * (-0.4986)
    g = x' * (-0.9689) + y' * 1.8758 + z' * 0.0415
    b = x' * 0.0557 + y' * (-0.2040) + z' * 1.0570

    r' = if r > 0.0031308 then 1.055 * r ** (1 / 2.4) - 0.055 else 12.92 * r
    g' = if g > 0.0031308 then 1.055 * g ** (1 / 2.4) - 0.055 else 12.92 * g
    b' = if b > 0.0031308 then 1.055 * b ** (1 / 2.4) - 0.055 else 12.92 * b

    r'' = round (max 0 (min 1 r') * 255)
    g'' = round (max 0 (min 1 g') * 255)
    b'' = round (max 0 (min 1 b') * 255)

-- Determines the squared error between two colors in LAB color space.
colorSquaredError :: CIELab -> CIELab -> Float
colorSquaredError (CIELab l1 a1 b1) (CIELab l2 a2 b2) =
  dl * dl + da * da + db * db
  where
    (dl, da, db) = (l1 - l2, a1 - a2, b1 - b2)

-- Averages a collection of colors in the LAB color space.
averageColor :: (Functor f, Foldable f) => f CIELab -> CIELab
averageColor cs
  | null cs = CIELab 0 0 0
  | otherwise = CIELab (l / n) (a / n) (b / n)
  where
    n = fromIntegral (length cs)
    CIELab l a b =
      foldr
        ( \(CIELab l1 a1 b1) (CIELab l2 a2 b2) ->
            CIELab (l1 + l2) (a1 + a2) (b1 + b2)
        )
        (CIELab 0 0 0)
        cs
