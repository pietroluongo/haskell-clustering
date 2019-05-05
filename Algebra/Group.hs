module Algebra.Group (
    Group(..),
    addToGroup,
    makeEmptyGroup
) where

import qualified Algebra.Point as AP

data Group = Group {
    centroid :: AP.Point,
    points :: [AP.Point]
} deriving (Show, Eq, Ord)

addToGroup :: AP.Point -> Group -> Group
addToGroup point group = Group (centroid group) (points (group) ++ [point])

makeEmptyGroup :: AP.Point -> Group
makeEmptyGroup point = Group point []