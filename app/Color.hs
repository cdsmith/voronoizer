module Color where

import Codec.Picture (PixelRGB8 (..))
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))
import Linear (Metric, Additive (..))
import GHC.Generics (Generic)

data CIELab a = CIELab !a !a !a
  deriving (Eq, Ord, Show, Functor, Foldable, Generic, Traversable)

instance Applicative CIELab where
  pure a = CIELab a a a
  CIELab f g h <*> CIELab a b c = CIELab (f a) (g b) (h c)

instance forall a. Storable a => Storable (CIELab a) where
  {-# SPECIALIZE instance Storable (CIELab Float) #-}

  sizeOf _ = 3 * sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)

  peek ptr = do
    l <- peek (castPtr ptr)
    a <- peekByteOff ptr (sizeOf (undefined :: a))
    b <- peekByteOff ptr (2 * sizeOf (undefined :: a))
    return $ CIELab l a b

  poke ptr (CIELab l a b) = do
    poke (castPtr ptr) l
    pokeByteOff ptr (sizeOf (undefined :: a)) a
    pokeByteOff ptr (2 * sizeOf (undefined :: a)) b

-- | Convert a JuicyPixel to a CIELab
--
-- Math from https://github.com/antimatter15/rgb-lab/blob/master/color.js
pixelToColor :: (Ord a, Floating a) => PixelRGB8 -> CIELab a
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
colorToPixel :: (Floating a, RealFrac a) => CIELab a -> PixelRGB8
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

instance Additive CIELab where
  zero = pure 0

instance Metric CIELab
