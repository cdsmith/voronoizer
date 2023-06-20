module Cost where

import Color (CIELab, averageColor, colorSquaredError)
import Data.KdTree.Dynamic (KdTree)
import Data.KdTree.Dynamic qualified as KdTree
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Geometry (BoundingBox, Point, atPoint, boxedPixels)
import Image (Grid (..))

meanSquaredError ::
  Grid CIELab ->
  BoundingBox ->
  KdTree Float Point ->
  Float
meanSquaredError colors box refPoints =
  getSum (foldMap mse proximities) / fromIntegral n
  where
    n = gridWidth colors * gridHeight colors
    proximities =
      Map.fromListWith
        (<>)
        [ (KdTree.nearest refPoints p, [atPoint colors p])
          | p <- boxedPixels box
        ]
    mse cs =
      let avg = averageColor cs
       in foldMap (Sum . colorSquaredError avg) cs

squaredSize :: KdTree Float Point -> Float
squaredSize refPoints = fromIntegral (n * n)
  where
    n = length refPoints

cost :: Grid CIELab -> BoundingBox -> KdTree Float Point -> Float
cost colors box refPoints =
  5000000 * meanSquaredError colors box refPoints
    + squaredSize refPoints
