module Voronoize where

import Color (CIELab (..))
import Data.KdTree.Static qualified as KdTree
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Foreign.Storable (Storable)
import Geometry
  ( Point (..),
    atPoint,
    boxedPixels,
    nearestNeighbors,
    pointCoords,
    pointSquareDist,
    wholeImage,
  )
import Image (Grid (..), generateGrid, gridAt)

data SumAndCount = SumAndCount !CIELab !Int

instance Semigroup SumAndCount where
  SumAndCount (CIELab l1 a1 b1) n1 <> SumAndCount (CIELab l2 a2 b2) n2 =
    SumAndCount (CIELab (l1 + l2) (a1 + a2) (b1 + b2)) (n1 + n2)

trivialSum :: CIELab -> SumAndCount
trivialSum c = SumAndCount c 1

toAverage :: SumAndCount -> CIELab
toAverage (SumAndCount (CIELab l a b) n) =
  CIELab (l / fromIntegral n) (a / fromIntegral n) (b / fromIntegral n)

voronoize :: Set Point -> Grid CIELab -> Grid CIELab
voronoize refPoints grid =
  generateGrid (gridWidth grid) (gridHeight grid) $
    \x y -> avgs Map.! gridAt nearestRefPoints x y
  where
    tree = KdTree.buildWithDist pointCoords pointSquareDist (Set.toList refPoints)
    nearestRefPoints = nearestNeighbors grid tree
    avgs = averageColors grid nearestRefPoints

averageColors ::
  (Ord key, Storable key) =>
  Grid CIELab ->
  Grid key ->
  Map key CIELab
averageColors colors keys =
  toAverage
    <$> Map.fromListWith
      (<>)
      [ (atPoint keys p, trivialSum (atPoint colors p))
        | p <- boxedPixels (wholeImage colors)
      ]
