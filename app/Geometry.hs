-- | This module defines the geometry of a 2D raster image, and provides
-- functions for manipulating it.
module Geometry where

import Data.KdTree.Static (KdTree)
import Data.KdTree.Static qualified as KdTree
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))
import Image (Grid (..), generateGrid, gridAt)

-- | A point on a 2D raster image.
data Point = Point !Float !Float
  deriving (Eq, Ord, Show)

instance Storable Point where
  sizeOf _ = 2 * sizeOf (undefined :: Float)
  alignment _ = alignment (undefined :: Float)

  peek ptr = do
    x <- peek (castPtr ptr)
    y <- peekByteOff ptr (sizeOf (undefined :: Float))
    return $ Point x y

  poke ptr (Point x y) = do
    poke (castPtr ptr) x
    pokeByteOff ptr (sizeOf (undefined :: Float)) y

-- | The coordinates of a Point.
pointCoords :: Point -> [Float]
pointCoords (Point x y) = [x, y]

-- | The square of the distance between two Points.
pointSquareDist :: Point -> Point -> Float
pointSquareDist (Point x1 y1) (Point x2 y2) = dx * dx + dy * dy
  where
    (dx, dy) = (x1 - x2, y1 - y2)

-- | The color of a pixel at a Point.
atPoint :: Storable a => Grid a -> Point -> a
atPoint grid (Point x y) = gridAt grid (floor x) (floor y)

-- | An axis-aligned bounding box on a 2D raster image.
data BoundingBox = BoundingBox !Int !Int !Int !Int
  deriving (Eq, Ord, Show)

instance Semigroup BoundingBox where
  BoundingBox x1 y1 x2 y2 <> BoundingBox x3 y3 x4 y4 =
    BoundingBox (min x1 x3) (min y1 y3) (max x2 x4) (max y2 y4)

-- | The bounding box that covers the entire image.
wholeImage :: Grid a -> BoundingBox
wholeImage grid = BoundingBox 0 0 (gridWidth grid - 1) (gridHeight grid - 1)

-- | The center point of a bounding box.
centerPoint :: BoundingBox -> Point
centerPoint (BoundingBox x1 y1 x2 y2) =
  Point
    ((fromIntegral x1 + fromIntegral x2) / 2)
    ((fromIntegral y1 + fromIntegral y2) / 2)

-- | The width of a bounding box in pixels.
boxWidth :: BoundingBox -> Int
boxWidth (BoundingBox x1 _ x2 _) = x2 - x1 + 1

-- | The height of a bounding box in pixels.
boxHeight :: BoundingBox -> Int
boxHeight (BoundingBox _ y1 _ y2) = y2 - y1 + 1

-- | The area of a bounding box in pixels.
boxedArea :: BoundingBox -> Int
boxedArea box = boxWidth box * boxHeight box

-- | The pixels contained in a bounding box.
boxedPixels :: BoundingBox -> [Point]
boxedPixels (BoundingBox x1 y1 x2 y2) =
  [Point (fromIntegral x) (fromIntegral y) | x <- [x1 .. x2], y <- [y1 .. y2]]

-- | Whether two bounding boxes overlap.
boxesOverlap :: BoundingBox -> BoundingBox -> Bool
boxesOverlap (BoundingBox x1 y1 x2 y2) (BoundingBox x3 y3 x4 y4) =
  x1 <= x4 && x2 >= x3 && y1 <= y4 && y2 >= y3

-- | Whether one bounding box is contained within another.
boxSubset :: BoundingBox -> BoundingBox -> Bool
boxSubset (BoundingBox x1 y1 x2 y2) (BoundingBox x3 y3 x4 y4) =
  x1 >= x3 && x2 <= x4 && y1 >= y3 && y2 <= y4

-- | Splits a bounding box along a vertical line, into a left and right
-- component.  The division is specified by the offset of the line along the x
-- axis.
splitAtXOffset :: BoundingBox -> Int -> (BoundingBox, BoundingBox)
splitAtXOffset (BoundingBox x1 y1 x2 y2) xoff =
  (BoundingBox x1 y1 (x1 + xoff - 1) y2, BoundingBox (x1 + xoff) y1 x2 y2)

-- | Splits a bounding box along a horizontal line, into a top and bottom
-- component.  The division is specified by the offset of the line along the y
-- axis.
splitAtYOffset :: BoundingBox -> Int -> (BoundingBox, BoundingBox)
splitAtYOffset (BoundingBox x1 y1 x2 y2) yoff =
  (BoundingBox x1 y1 x2 (y1 + yoff - 1), BoundingBox x1 (y1 + yoff) x2 y2)

-- Returns a Grid of the nearest neighboring reference point to each pixel in
-- the given image.
nearestNeighbors :: Grid a -> KdTree Float Point -> Grid Point
nearestNeighbors grid tree = generateGrid width height pixel
  where
    width = gridWidth grid
    height = gridHeight grid
    pixel x y = KdTree.nearest tree (Point (fromIntegral x) (fromIntegral y))
