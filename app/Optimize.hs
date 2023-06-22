module Optimize where

import Data.List (maximumBy)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPrio

type Interval = (Float, Float)

intervalSize :: Interval -> Float
intervalSize (lo, hi) = hi - lo

intervalMidpoint :: Interval -> Float
intervalMidpoint (lo, hi) = (lo + hi) / 2

data OptimizationParams = OptimizationParams
  { optimizeTopK :: Int,
    optimizeCloseEnough :: Float
  }
  deriving (Show)

data Model = Model
  { modelParams :: OptimizationParams,
    modelRange :: Interval,
    modelSamplesByX :: Map.Map Float Float,
    modelSamplesByY :: MinPQueue Float Float
  }
  deriving (Show)

nextIntervalToExplore :: Model -> Interval -> Interval
nextIntervalToExplore model (lo, hi) =
  let inRange x = x >= lo && x <= hi
      xs = lo : filter inRange (Map.keys (modelSamplesByX model)) ++ [hi]
      gaps = zip xs (tail xs)
   in maximumBy (comparing intervalSize) gaps

data Next a = FinalAnswer a | NextTrial a

emptyModel :: OptimizationParams -> Interval -> Model
emptyModel params range = Model params range Map.empty MinPrio.empty

next :: Model -> Next Float
next model
  | intervalSize nextInterval < closeEnough =
      FinalAnswer $
        if MinPrio.null (modelSamplesByY model)
          then intervalMidpoint (modelRange model)
          else snd (MinPrio.findMin (modelSamplesByY model))
  | otherwise = NextTrial (intervalMidpoint nextInterval)
  where
    k = optimizeTopK (modelParams model)
    closeEnough = optimizeCloseEnough (modelParams model)
    topK = snd <$> take k (MinPrio.toAscList (modelSamplesByY model))
    targetInterval =
      if length (modelSamplesByY model) < k
        then modelRange model
        else (minimum topK, maximum topK)
    nextInterval = nextIntervalToExplore model targetInterval

updateModel :: Float -> Float -> Model -> Model
updateModel x y model =
  model
    { modelSamplesByX = Map.insert x y (modelSamplesByX model),
      modelSamplesByY = MinPrio.insert y x (modelSamplesByY model)
    }

minimize :: OptimizationParams -> (Float -> Float) -> Interval -> Float
minimize params f range = go (emptyModel params range)
  where
    go model = case next model of
      FinalAnswer x -> x
      NextTrial x -> go (updateModel x (f x) model)
