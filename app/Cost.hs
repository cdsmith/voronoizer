module Cost where

import Color (CIELab)
import Data.KdTree.Dynamic (KdTree)
import Data.KdTree.Dynamic qualified as KdTree
import Data.Map.Strict qualified as Map
import Foreign.Storable (Storable)
import Geometry (BoundingBox, Point, atPoint, boxedPixels)
import Image (Grid (..))
import Stats (observation, sampleSize, sampleVariance)

meanSquaredError ::
  (Floating a, Real a, Storable a) =>
  Grid (CIELab a) ->
  BoundingBox ->
  KdTree a Point ->
  a
meanSquaredError colors box refPoints =
  sum (zipWith (*) weights vars) / sum weights
  where
    stats =
      Map.elems
        ( Map.fromListWith
            (<>)
            [ (KdTree.nearest refPoints p, observation (atPoint colors p))
              | p <- boxedPixels box
            ]
        )
    weights = fromIntegral . sampleSize <$> stats
    vars = sampleVariance <$> stats

squaredSize :: Num a => KdTree a Point -> a
squaredSize refPoints = fromIntegral (n * n)
  where
    n = length refPoints

cost ::
  (Floating a, Real a, Storable a) =>
  Grid (CIELab a) ->
  BoundingBox ->
  KdTree a Point ->
  a
cost colors box refPoints =
  5000000 * meanSquaredError colors box refPoints + squaredSize refPoints
