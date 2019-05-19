module Algebra.Group (
    Group(..),
    makeEmptyGroup,
    findGroupByCentroid,
    groupPoints,
    updateGroups,
    getTotalSSE,
    groupPointsFirstIter,
    groupPointsIter
) where

import qualified Algebra.Point as AP
import Data.List
import Data.Function

__CONST_MAX_IT = 100

data Group = Group {
    centroid :: AP.Point,
    points :: [AP.Point]
} deriving (Show, Eq, Ord)

-- Function that adds a list of points to a group
-- Parameters:
--     pointsToAdd: [Point] -> Points to be added
--     group: Group -> Group to add the points to
-- Result:
--     Group -> Group with the points added
addToGroup :: [AP.Point] -> Group -> Group
addToGroup pointsToAdd group = Group (centroid group) (points (group) ++ pointsToAdd)

-- Function that creates an empty group
-- Parameters:
--     point: Point -> Points to be the centroid of the group
-- Result:
--     Group -> Empty group with the specified centroid
makeEmptyGroup :: AP.Point -> Group
makeEmptyGroup point = Group point []

-- Function that finds a group given a specified centroid
-- Parameters:
--     groups: [Group] -> List of groups to search from
--     cent: Point -> Centroid of desired group
-- Result:
--     Group -> Group with determined centroid
findGroupByCentroid :: [Group] -> AP.Point -> Group
findGroupByCentroid groups cent = head [x | x <- groups, centroid x == cent]

-- Function that calculates the total SSE of a group
-- Parameters:
--     groups: [Group] -> Groups to calculate the SSE from
-- Result:
--     Double -> Calculated SSE
getTotalSSE :: [Group] -> Double
getTotalSSE groups = total
    where
        a = map (\group -> AP.sse (points group) (centroid group)) groups
        total = sum a


--FIXME
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

-- Function that groups points together. This is the main grouping function.
-- Parameters:
--     points: [[(Point, Point)]] -> (Point, Centroid) - Points that must be grouped with their respective centroid
-- Result:
--     [Group] -> Calculated groups
-- FIXME
groupPointsFirstIter :: [AP.Point] -> [AP.Point] -> [Group]
groupPointsFirstIter points centroids = groups
    where
        zipped = zip points (map (AP.findNearest centroids) points)
        groups = groupPoints zipped


-- Function that groups points together.
-- Parameters:
--     points: [[(Point, Point)]] -> (Point, Centroid) - Points that must be grouped with their respective centroid
-- Result:
--     [Group] -> Calculated groups
-- FIXME
groupPoints :: [(AP.Point, AP.Point)] -> [Group]
groupPoints points = z
    where
        x = groupBy ((==) `on` snd) (sortBy (compare `on` snd) points)
        z = updateGroups x

-- Function that updates a group with a list of points
-- Parameters:
--     info: [[(Point, Point)]] -> (Point, Centroid) - Makes a group with a Centroid and adds Point to it. 
--                                                     The tuples must be sorted and grouped beforehand.
-- Result:
--     [Group] -> List of groups that have the determined centroids
-- FIXME
updateGroups :: [[(AP.Point, AP.Point)]] -> [Group]
updateGroups dados = c
    where
        a = head $ tail dados
        b = Group (snd $ head a) [fst x | x <- a]
        c = map (\q -> Group (snd $ head q) [fst x | x <- q]) dados