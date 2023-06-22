{-# LANGUAGE ViewPatterns #-}

module Divide where

import Color (CIELab)
import Data.Set (Set)
import Data.Set qualified as Set
import Geometry
  ( BoundingBox (..),
    Point (..),
    atPoint,
    boxHeight,
    boxSubset,
    boxWidth,
    boxedArea,
    boxedPixels,
    boxesOverlap,
    centerPoint,
    pointSquareDist,
    splitAtXOffset,
    splitAtYOffset,
    wholeImage,
  )
import Image
  ( Grid,
    fromImage,
    getImage,
    saveImage,
    toImage,
  )
import Optimize (OptimizationParams (..), minimize)
import Stats
  ( StatTree (..),
    buildStatTree,
    observation,
    sampleVariance,
    treeRegion,
    treeStats,
  )
import System.Environment (getArgs)
import Voronoize (voronoize)

doOptimize :: IO ()
doOptimize = do
  [input, output, read -> n, read -> u, read -> f] <- getArgs
  img <- getImage input
  let colors = fromImage img
  let params = estimateDivisionParams n u f colors
  let box = wholeImage colors
  let points = chooseReferencePoints params colors (longerDir box) box
  putStrLn $
    "Saving "
      <> output
      <> " with "
      <> show (length points)
      <> " reference points."
  saveImage output (toImage (voronoize points colors))

data DivisionParams = DivisionParams
  { targetArea :: Float,
    targetMSE :: Float,
    uniformity :: Float,
    focus :: Float
  }

estimateDivisionParams ::
  Int -> Float -> Float -> Grid (CIELab Float) -> DivisionParams
estimateDivisionParams n u f colors =
  DivisionParams
    { targetArea =
        fromIntegral (boxedArea (wholeImage colors)) / fromIntegral n,
      targetMSE =
        sum [colorVariance colors box | box <- boxes]
          / fromIntegral (length boxes),
      uniformity = u,
      focus = f
    }
  where
    boxes = equalAreaBoxes (wholeImage colors) n

equalAreaBoxes :: BoundingBox -> Int -> [BoundingBox]
equalAreaBoxes whole@(BoundingBox x1 y1 x2 y2) n =
  [ BoundingBox
      (x1 + round (dx * fromIntegral i))
      (y1 + round (dy * fromIntegral j))
      (min x2 (x1 + round (dx * fromIntegral (i + 1)) - 1))
      (min y2 (y1 + round (dy * fromIntegral (j + 1)) - 1))
    | i <- [0 .. rows - 1],
      j <- [0 .. cols - 1]
  ]
  where
    rows = ceiling (sqrt (fromIntegral n) :: Float) :: Int
    cols = ceiling (fromIntegral n / fromIntegral rows :: Float) :: Int
    dx = fromIntegral (boxWidth whole) / fromIntegral cols :: Float
    dy = fromIntegral (boxHeight whole) / fromIntegral rows :: Float

chooseReferencePoints ::
  DivisionParams -> Grid (CIELab Float) -> Direction -> BoundingBox -> Set Point
chooseReferencePoints params colors dir box
  | shouldStop params colors colorStats = Set.singleton (centerPoint box)
  | otherwise =
      foldMap
        (chooseReferencePoints params colors (orthogonal dir))
        (divide dir params colors colorStats)
  where
    colorStats = computeColorStats dir colors box

shouldStop ::
  DivisionParams ->
  Grid (CIELab Float) ->
  StatTree BoundingBox CIELab Float ->
  Bool
shouldStop params colors colorStats =
  fromIntegral (boxedArea (treeRegion colorStats)) < areaThreshold params
    || sampleVariance (treeStats colorStats)
      < errorThreshold params colors (treeRegion colorStats)

areaThreshold :: DivisionParams -> Float
areaThreshold params = targetArea params / 10

computeColorStats ::
  Direction ->
  Grid (CIELab Float) ->
  BoundingBox ->
  StatTree BoundingBox CIELab Float
computeColorStats dir colors = buildStatTree measure partition
  where
    measure box = foldMap (observation . atPoint colors) (boxedPixels box)
    partition box
      | boxDimension dir box < 2 = Nothing
      | otherwise = Just (splitAtOffset dir box (boxDimension dir box `div` 2))

colorVariance :: Grid (CIELab Float) -> BoundingBox -> Float
colorVariance colors box =
  sampleVariance (foldMap (observation . atPoint colors) (boxedPixels box))

errorThreshold :: DivisionParams -> Grid (CIELab Float) -> BoundingBox -> Float
errorThreshold params colors box =
  targetMSE params
    * ((1 + targetArea params) / (1 + fromIntegral (boxedArea box)))
      ** uniformity params
    / focalFactor
  where
    focalDistance =
      (2 * pointSquareDist (centerPoint box) (centerPoint (wholeImage colors)))
        / fromIntegral (boxedArea (wholeImage colors))
    focalFactor =
      (1 / focus params + focus params) * focalDistance * focalDistance
        + focus params

data Direction = Vertical | Horizontal

orthogonal :: Direction -> Direction
orthogonal Vertical = Horizontal
orthogonal Horizontal = Vertical

longerDir :: BoundingBox -> Direction
longerDir box
  | boxWidth box > boxHeight box = Horizontal
  | otherwise = Vertical

boxDimension :: Direction -> BoundingBox -> Int
boxDimension Vertical = boxHeight
boxDimension Horizontal = boxWidth

splitAtOffset :: Direction -> BoundingBox -> Int -> (BoundingBox, BoundingBox)
splitAtOffset Vertical = splitAtYOffset
splitAtOffset Horizontal = splitAtXOffset

divide ::
  Direction ->
  DivisionParams ->
  Grid (CIELab Float) ->
  StatTree BoundingBox CIELab Float ->
  [BoundingBox]
divide dir params colors colorStats = boxes bestOffset
  where
    whole = treeRegion colorStats
    minOffset =
      min (boxDimension dir whole `div` 2) . max 2 . round $
        areaThreshold params / fromIntegral (boxDimension (orthogonal dir) whole) / 2
    bestOffset =
      minimize
        (OptimizationParams {optimizeTopK = 4, optimizeCloseEnough = 4})
        totalCost
        ( fromIntegral minOffset,
          fromIntegral (boxDimension dir whole - minOffset)
        )

    boxStats (LeafStats leafBox leafStats) box
      | leafBox `boxSubset` box = leafStats
      | otherwise = mempty
    boxStats (BranchStats leftTree branchBox branchStats rightTree) box
      | branchBox `boxSubset` box = branchStats
      | boxesOverlap branchBox box =
          boxStats leftTree box <> boxStats rightTree box
      | otherwise = mempty

    boxCost b =
      sampleVariance (boxStats colorStats b)
        / errorThreshold params colors b
    totalCost offset = sum (boxCost <$> boxes offset)
    boxes offset = [a, b]
      where
        (a, b) = splitAtOffset dir whole (round offset)
