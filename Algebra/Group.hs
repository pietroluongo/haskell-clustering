module Algebra.Group (
    Group(..),
    addToGroup,
    makeEmptyGroup,
    findGroupByCentroid,
    groupPoints,
    updateGroups
) where

import qualified Algebra.Point as AP
import Data.List
import Data.Function

data Group = Group {
    centroid :: AP.Point,
    points :: [AP.Point]
} deriving (Show, Eq, Ord)

addToGroup :: [AP.Point] -> Group -> Group
addToGroup pointsToAdd group = Group (centroid group) (points (group) ++ pointsToAdd)

makeEmptyGroup :: AP.Point -> Group
makeEmptyGroup point = Group point []

findGroupByCentroid :: [Group] -> AP.Point -> Group
findGroupByCentroid groups c = head [x | x <- groups, centroid x == c]

updateGroups :: [[(AP.Point, AP.Point)]] -> [Group]
updateGroups info = c
    where
        a = head $ tail info
        b = Group (snd $ head a) [fst x | x <- a]
        c = map (\q -> Group (snd $ head q) [fst x | x <- q]) info

groupPoints :: [(AP.Point, AP.Point)] -> [Group]
groupPoints points = z
    where
        x = groupBy ((==) `on` snd) (sortBy (compare `on` snd) points)
        z =  updateGroups x