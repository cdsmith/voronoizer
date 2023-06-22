module Stats where

import Color (CIELab)
import Linear (Metric, norm, zero, (*^), (^+^), (^-^), (^/))

data Stats f a = Stats
  { sampleSize :: !Int,
    sampleMean :: !(f a),
    sampleSqDist :: !a
  }
  deriving Show

instance (Metric f, Floating a) => Semigroup (Stats f a) where
  {-# SPECIALIZE instance Semigroup (Stats CIELab Float) #-}

  (Stats n1 m1 sqd1) <> (Stats n2 m2 sqd2) =
    Stats n m sqd
    where
      n = n1 + n2
      delta = m2 ^-^ m1
      m =
        (fromIntegral n1 *^ m1 ^+^ fromIntegral n2 *^ m2)
          ^/ fromIntegral n
      sqd = sqd1 + sqd2 + norm delta * fromIntegral (n1 * n2) / fromIntegral n

instance (Metric f, Floating a) => Monoid (Stats f a) where
  mempty = Stats 0 zero 0

observation :: (Metric f, Floating a) => f a -> Stats f a
observation v = Stats 1 v 0

sampleVariance :: (Metric f, Floating a) => Stats f a -> a
sampleVariance (Stats n _ sqd) = sqd / fromIntegral n

data StatTree region f a
  = LeafStats region (Stats f a)
  | BranchStats (StatTree region f a) region (Stats f a) (StatTree region f a)
  deriving Show

treeStats :: (Metric f, Floating a) => StatTree region f a -> Stats f a
treeStats (LeafStats _ stats) = stats
treeStats (BranchStats _ _ stats _) = stats

treeRegion :: StatTree region f a -> region
treeRegion (LeafStats region _) = region
treeRegion (BranchStats _ region _ _) = region

buildStatTree ::
  (Metric f, Floating a) =>
  (region -> Stats f a) ->
  (region -> Maybe (region, region)) ->
  region ->
  StatTree region f a
buildStatTree measure divide region =
  case divide region of
    Nothing -> LeafStats region (measure region)
    Just (left, right) ->
      let leftTree = buildStatTree measure divide left
          rightTree = buildStatTree measure divide right
          stats = treeStats leftTree <> treeStats rightTree
       in BranchStats leftTree region stats rightTree
