module Algebra.Group (
    Group(..)
) where

import Algebra.Point

data Group = Group {
    centroid :: Point,
    points :: [Point]
} deriving (Show, Eq, Ord)

