module Algebra.Group (
    Group(..),
    addToGroup,
    makeEmptyGroup,
    findGroupByCentroid,
    groupPoints,
    updateGroups,
    getTotalSSE,
    groupPointsIter,
    groupPointsFirstIter
) where

import qualified Algebra.Point as AP
import Data.List
import Data.Function

__CONST_MAX_IT = 100

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
        z = updateGroups x

getTotalSSE :: [Group] -> Double
getTotalSSE groups = total
    where
        a = map (\group -> AP.sse (points group) (centroid group)) groups
        total = sum a

groupPointsIter :: [Group] -> Int -> [Group]
groupPointsIter groups cur_iter
    | cur_iter == __CONST_MAX_IT = groups
    | not hasChanged = groups
    | otherwise = groupPointsIter newGroups (cur_iter+1)
    where
        pontos = concat $ map (points) groups
        updatedCentroids = map AP.findCentroid (map (points) groups)
        nearestCentroids = (map (AP.findNearest updatedCentroids) pontos)
        zipped = zip pontos nearestCentroids
        newGroups = groupPoints zipped
        hasChanged = groups /= newGroups


groupPointsFirstIter :: [AP.Point] -> [AP.Point] -> [Group]
groupPointsFirstIter points centroids = groups
    where
        zipped = zip points (map (AP.findNearest centroids) points)
        groups = groupPoints zipped