-- | This module defines the geometry of a 2D raster image, and provides
-- functions for manipulating it.
module Geometry where

import Data.KdTree.Dynamic (KdTree)
import Data.KdTree.Dynamic qualified as KdTree
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))
import Image (Grid (..), generateGrid, gridAt)

-- | A point on a 2D raster image.
data Point = Point !Int !Int
  deriving (Eq, Ord, Show)

instance Storable Point where
  sizeOf _ = 2 * sizeOf (undefined :: Int)
  alignment _ = alignment (undefined :: Int)

  peek ptr = do
    x <- peek (castPtr ptr)
    y <- peekByteOff ptr (sizeOf (undefined :: Int))
    return $ Point x y

  poke ptr (Point x y) = do
    poke (castPtr ptr) x
    pokeByteOff ptr (sizeOf (undefined :: Int)) y

-- | The coordinates of a Point.
pointCoords :: Point -> [Float]
pointCoords (Point x y) = [fromIntegral x, fromIntegral y]

-- | The square of the distance between two Points.
pointSquareDist :: Point -> Point -> Float
pointSquareDist (Point x1 y1) (Point x2 y2) = fromIntegral (dx * dx + dy * dy)
  where
    (dx, dy) = (x1 - x2, y1 - y2)

-- | The color of a pixel at a Point.
atPoint :: (Storable a) => Grid a -> Point -> a
atPoint grid (Point x y) = gridAt grid x y

-- | An axis-aligned bounding box on a 2D raster image.
data BoundingBox = BoundingBox !Point !Point
  deriving (Eq, Ord, Show)

-- | The bounding box that covers the entire image.
wholeImage :: Grid a -> BoundingBox
wholeImage grid = BoundingBox (Point 0 0) (Point (width - 1) (height - 1))
  where
    width = gridWidth grid
    height = gridHeight grid

-- | The center point of a bounding box.
centerPoint :: BoundingBox -> Point
centerPoint (BoundingBox (Point x1 y1) (Point x2 y2)) =
  Point ((x1 + x2) `div` 2) ((y1 + y2) `div` 2)

-- | The width of a bounding box in pixels.
boxWidth :: BoundingBox -> Int
boxWidth (BoundingBox (Point x1 _) (Point x2 _)) = x2 - x1 + 1

-- | The height of a bounding box in pixels.
boxHeight :: BoundingBox -> Int
boxHeight (BoundingBox (Point _ y1) (Point _ y2)) = y2 - y1 + 1

-- | The area of a bounding box in pixels.
boxedArea :: BoundingBox -> Float
boxedArea (BoundingBox (Point x1 y1) (Point x2 y2)) =
  fromIntegral ((x2 - x1 + 1) * (y2 - y1 + 1))

-- | The pixels contained in a bounding box.
boxedPixels :: BoundingBox -> [Point]
boxedPixels (BoundingBox (Point x1 y1) (Point x2 y2)) =
  [Point x y | x <- [x1 .. x2], y <- [y1 .. y2]]

-- | Splits a bounding box along a vertical line, into a left and right
-- component.  The division is specified by the offset of the line along the x
-- axis.
splitAtXOffset :: BoundingBox -> Int -> (BoundingBox, BoundingBox)
splitAtXOffset (BoundingBox (Point x1 y1) (Point x2 y2)) xm =
  ( BoundingBox (Point x1 y1) (Point (x1 + xm - 1) y2),
    BoundingBox (Point (x1 + xm) y1) (Point x2 y2)
  )

-- | Splits a bounding box along a horizontal line, into a top and bottom
-- component.  The division is specified by the offset of the line along the y
-- axis.
splitAtYOffset :: BoundingBox -> Int -> (BoundingBox, BoundingBox)
splitAtYOffset (BoundingBox (Point x1 y1) (Point x2 y2)) ym =
  ( BoundingBox (Point x1 y1) (Point x2 (y1 + ym - 1)),
    BoundingBox (Point x1 (y1 + ym)) (Point x2 y2)
  )

-- | An empty KdTree of Points.
emptyKdTree :: KdTree Float Point
emptyKdTree = KdTree.emptyWithDist pointCoords pointSquareDist

-- Returns a Grid of the nearest neighboring reference point to each pixel in
-- the given image.
nearestNeighbors :: Grid a -> KdTree Float Point -> Grid Point
nearestNeighbors grid tree = generateGrid width height pixel
  where
    width = gridWidth grid
    height = gridHeight grid
    pixel x y = KdTree.nearest tree (Point x y)
