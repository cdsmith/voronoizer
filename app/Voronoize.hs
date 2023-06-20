module Voronoize where

import Color (CIELab (..))
import Data.KdTree.Dynamic (KdTree)
import Data.Map.Strict qualified as Map
import Geometry (Point (..), atPoint, boxedPixels, nearestNeighbors, wholeImage)
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

voronoize ::
  KdTree Float Point ->
  Grid CIELab ->
  Grid CIELab
voronoize refPoints grid = generateGrid (gridWidth grid) (gridHeight grid) $
  \x y ->
    avgColors Map.! gridAt nearestRefPoints x y
  where
    nearestRefPoints = nearestNeighbors grid refPoints
    avgColors =
      toAverage
        <$> Map.fromListWith
          (<>)
          [ (atPoint nearestRefPoints p, trivialSum (atPoint grid p))
            | p <- boxedPixels (wholeImage grid)
          ]
