module FineTune where

import Codec.Picture (Image (..))
import Color (CIELab)
import Control.Monad (when)
import Cost (cost)
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.KdTree.Dynamic (KdTree)
import Data.KdTree.Dynamic qualified as KdTree
import Data.List qualified as List
import Data.Ord (comparing)
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPrio
import Geometry (Point (..), pointCoords, pointSquareDist, wholeImage)
import Image (Grid, fromImage, getImage, saveImage, toImage)
import System.Environment (getArgs)
import System.Random (randomRIO)
import Voronoize (voronoize)

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

doOptimize :: IO ()
doOptimize = do
  [input] <- getArgs
  img <- getImage input
  let cs = fromImage img
  go (1 :: Int) img cs startingCandidates
  where
    go i img cs candidates = do
      putStrLn "Selecting..."
      refPoints <- select candidates
      putStrLn "Mutating..."
      refPoints' <- mutate (imageWidth img) (imageHeight img) refPoints
      putStrLn "Augmenting..."
      let candidates' = augment refPoints' cs candidates
      summarize candidates'
      when (i `mod` 100 == 0) $ do
        let best =
              List.minimumBy
                (comparing fst)
                (MinPrio.findMin <$> IntMap.elems (unCandidates candidates'))
        let outFile = "out-" <> show i <> ".png"
        putStrLn $ "Writing best score (" <> show (fst best) <> ") to " <> outFile
        let cs' = voronoize (snd best) cs
        saveImage outFile (toImage cs')
      go (i + 1) img cs candidates'
