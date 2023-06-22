{-# LANGUAGE ViewPatterns #-}

module Divide where

import Color (CIELab, averageColor, colorSquaredError)
import Data.Set (Set)
import Data.Set qualified as Set
import Geometry
  ( BoundingBox (..),
    Point (..),
    atPoint,
    boxHeight,
    boxWidth,
    boxedArea,
    boxedPixels,
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

estimateDivisionParams :: Int -> Float -> Float -> Grid CIELab -> DivisionParams
estimateDivisionParams n u f colors =
  DivisionParams
    { targetArea = boxedArea (wholeImage colors) / fromIntegral n,
      targetMSE =
        sum [meanSquaredError colors box | box <- boxes]
          / fromIntegral (length boxes),
      uniformity = u,
      focus = f
    }
  where
    boxes = equalAreaBoxes (wholeImage colors) n

equalAreaBoxes :: BoundingBox -> Int -> [BoundingBox]
equalAreaBoxes whole@(BoundingBox (Point x1 y1) (Point x2 y2)) n =
  [ BoundingBox
      ( Point
          (x1 + round (dx * fromIntegral i))
          (y1 + round (dy * fromIntegral j))
      )
      ( Point
          (min x2 (x1 + round (dx * fromIntegral (i + 1))))
          (min y2 (y1 + round (dy * fromIntegral (j + 1))))
      )
    | i <- [0 .. rows - 1],
      j <- [0 .. cols - 1]
  ]
  where
    rows = ceiling (sqrt (fromIntegral n) :: Float) :: Int
    cols = ceiling (fromIntegral n / fromIntegral rows :: Float) :: Int
    dx = fromIntegral (boxWidth whole) / sqrt (fromIntegral n) :: Float
    dy = boxedArea whole / fromIntegral n / dx :: Float

chooseReferencePoints ::
  DivisionParams -> Grid CIELab -> Direction -> BoundingBox -> Set Point
chooseReferencePoints params colors dir box
  | shouldStop params colors box = Set.singleton (centerPoint box)
  | otherwise =
      foldMap
        (chooseReferencePoints params colors (orthogonal dir))
        (divide dir params colors box)

shouldStop :: DivisionParams -> Grid CIELab -> BoundingBox -> Bool
shouldStop params colors box =
  boxedArea box < areaThreshold params
    || meanSquaredError colors box < errorThreshold params colors box

areaThreshold :: DivisionParams -> Float
areaThreshold params = targetArea params / 10

meanSquaredError :: Grid CIELab -> BoundingBox -> Float
meanSquaredError colors box =
  sum (colorSquaredError avgColor <$> boxedColors)
    / fromIntegral (length boxedColors)
  where
    boxedColors = atPoint colors <$> boxedPixels box
    avgColor = averageColor boxedColors

errorThreshold :: DivisionParams -> Grid CIELab -> BoundingBox -> Float
errorThreshold params colors box =
  targetMSE params
    * ((1 + targetArea params) / (1 + boxedArea box)) ** uniformity params
    / focalFactor
  where
    focalDistance =
      (2 * pointSquareDist (centerPoint box) (centerPoint (wholeImage colors)))
        / boxedArea (wholeImage colors)
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
  Direction -> DivisionParams -> Grid CIELab -> BoundingBox -> [BoundingBox]
divide dir params colors box = boxes bestOffset
  where
    minOffset =
      min (fromIntegral (boxDimension dir box) - 1) . max 2 $
        areaThreshold params / fromIntegral (boxDimension (orthogonal dir) box) / 2
    bestOffset =
      minimize
        (OptimizationParams {optimizeTopK = 4, optimizeCloseEnough = 4})
        totalCost
        (minOffset, fromIntegral (boxDimension dir box) - minOffset - 1)
    boxCost b = meanSquaredError colors b / errorThreshold params colors b
    totalCost offset = sum (boxCost <$> boxes offset)
    boxes offset = [a, b]
      where
        (a, b) = splitAtOffset dir box (round offset)
