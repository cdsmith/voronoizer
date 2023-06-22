module FineTune where

import Geometry (Point)

data Mutation
  = -- | Add a new point within this point's cell.
    AddPoint Point
  | -- | Remove the given point.
    RemovePoint Point
  | -- | Move the given point in the direction of the cell's centroid.
    LloydRelax Point
  | -- | Merge adjacent points whose cells have similar colors.
    MergePoints Point Point
  | -- | Adjust adjacent points to match an edge between them.
    MatchEdge Point Point

