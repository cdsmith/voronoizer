{-# LANGUAGE ViewPatterns #-}

module Divide where

import Color (CIELab, averageColor, colorSquaredError)
import Data.Foldable (foldl')
import Data.KdTree.Dynamic (KdTree)
import Data.KdTree.Dynamic qualified as KdTree
import Geometry
  ( BoundingBox (..),
    Point (..),
    atPoint,
    boxHeight,
    boxWidth,
    boxedArea,
    boxedPixels,
    centerPoint,
    emptyKdTree,
    pointCoords,
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
import System.Environment (getArgs)
import Voronoize (voronoize)

doOptimize :: IO ()
doOptimize = do
  [input, output, read -> n, read -> u, read -> f] <- getArgs
  img <- getImage input
  let colors = fromImage img
  let params = estimateDivisionParams n u f colors
  let points = chooseReferencePoints params colors (wholeImage colors)
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
  DivisionParams -> Grid CIELab -> BoundingBox -> KdTree Float Point
chooseReferencePoints params colors box
  | boxedArea box < areaThreshold || errorScore params colors box < 1 =
      KdTree.singletonWithDist pointCoords pointSquareDist (centerPoint box)
  | otherwise =
      foldl'
        (\t !b -> foldl' (\t' !p -> KdTree.insert t' p) t (chooseReferencePoints params colors b))
        emptyKdTree
        (divide params colors box)
  where
    areaThreshold = targetArea params / 10

divide :: DivisionParams -> Grid CIELab -> BoundingBox -> [BoundingBox]
divide params colors box
  | boxWidth box > boxHeight box = divideHorizontally params colors box
  | otherwise = divideVertically params colors box

divideHorizontally :: DivisionParams -> Grid CIELab -> BoundingBox -> [BoundingBox]
divideHorizontally params colors box = go 0 (boxWidth box)
  where
    go x width
      | width < 4 = [left, right]
      | leftErr < rightErr = go (x + halfWidth) halfWidth
      | otherwise = go x halfWidth
      where
        halfWidth = width `div` 2
        (left, right) = splitAtXOffset box (x + halfWidth)
        leftErr = errorScore params colors left
        rightErr = errorScore params colors right

divideVertically :: DivisionParams -> Grid CIELab -> BoundingBox -> [BoundingBox]
divideVertically params colors box = go 0 (boxHeight box)
  where
    go y height
      | height < 4 = [top, bottom]
      | topErr < bottomErr = go (y + halfHeight) halfHeight
      | otherwise = go y halfHeight
      where
        halfHeight = height `div` 2
        (top, bottom) = splitAtYOffset box (y + halfHeight)
        topErr = errorScore params colors top
        bottomErr = errorScore params colors bottom

meanSquaredError :: Grid CIELab -> BoundingBox -> Float
meanSquaredError colors box =
  sum (colorSquaredError avgColor <$> boxedColors)
    / fromIntegral (length boxedColors)
  where
    boxedColors = atPoint colors <$> boxedPixels box
    avgColor = averageColor boxedColors

errorScore :: DivisionParams -> Grid CIELab -> BoundingBox -> Float
errorScore params colors box = meanSquaredError colors box / errorThreshold
  where
    focalDistance =
      (2 * pointSquareDist (centerPoint box) (centerPoint (wholeImage colors)))
        / boxedArea (wholeImage colors)
    focalFactor = (1 / focus params + focus params) * focalDistance * focalDistance + focus params
    errorThreshold =
      targetMSE params
        * ((1 + targetArea params) / (1 + boxedArea box)) ** uniformity params
        / focalFactor
