module FineTune where

import Color (CIELab)
import Cost (cost)
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.KdTree.Dynamic (KdTree)
import Data.KdTree.Dynamic qualified as KdTree
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPrio
import Geometry (Point (..), pointCoords, pointSquareDist, wholeImage)
import Image (Grid)
import System.Random (randomRIO)

newtype Candidates = Candidates
  { unCandidates :: IntMap (MinPQueue Float (KdTree Float Point))
  }

startingCandidates :: Candidates
startingCandidates =
  Candidates
    (IntMap.singleton 0 (MinPrio.singleton 1e50 emptyKdTree))
  where
    emptyKdTree = KdTree.emptyWithDist pointCoords pointSquareDist

randomBiasedIO :: Float -> (Int, Int) -> IO Int
randomBiasedIO p (lo, hi) = do
  let r = fromIntegral (hi - lo + 1)
  u <- randomRIO (0, r ** p) :: IO Float
  pure (lo + floor (u ** (1 / p)))

select :: Candidates -> IO (KdTree Float Point)
select (Candidates cs) = do
  i <- randomBiasedIO 3 (0, length cs - 1)
  let sized = cs IntMap.! i

  u <- randomRIO (0, 1) :: IO Float
  let p = 0.5 -- Probability of choosing best candidate
  let j = min (length sized - 1) (ceiling (logBase (1 - p) (1 - u)) - 1)
  pure (snd (MinPrio.findMin (MinPrio.drop j sized)))

mutate :: Int -> Int -> KdTree Float Point -> IO (KdTree Float Point)
mutate width height refPoints = do
  x <- randomRIO (0, width - 1)
  y <- randomRIO (0, height - 1)
  let p = Point x y
  pure (KdTree.insert refPoints p)

augment ::
  KdTree Float Point -> Grid CIELab -> Candidates -> Candidates
augment refPoints colors (Candidates cs) =
  Candidates
    ( IntMap.insertWith
        (<>)
        (length refPoints)
        (MinPrio.singleton (cost colors (wholeImage colors) refPoints) refPoints)
        cs
    )

summarize :: Candidates -> IO ()
summarize (Candidates cs) = do
  putStrLn "Candidates:"
  mapM_ (uncurry summarizeSize) (IntMap.toList cs)
  where
    summarizeSize :: Int -> MinPQueue Float (KdTree Float Point) -> IO ()
    summarizeSize i q =
      putStrLn $
        show i
          <> ": "
          <> show (length q)
          <> " choices.  Best score = "
          <> show (fst (MinPrio.findMin q))
