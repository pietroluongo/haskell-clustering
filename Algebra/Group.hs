module Algebra.Group (
    Group(..),
    getTotalSSE,
    groupPoints
) where

import qualified Algebra.Point as AP
import Data.List
import Data.Function

-- Constant: Max possible iteration count
maxIterations = 100

-- Abstract type "Group"
-- Fields:
--     centroid: Point -> Centroid from the group.
--     points: [Point] -> Points that compose the group.
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


-- Function that forms groups. This is the main grouping function.
-- Parameters:
--     points: [Point] -> List of points to be grouped
--     centroids: [Point] -> List of centroids that groups should be formed around
-- Result:
--     [Group] -> Formed groups after all required iterations
groupPoints :: [AP.Point] -> [AP.Point] -> [Group]
groupPoints points centroids = finalGroups
    where
        zipped = zip points (map (AP.findNearest centroids) points)
        groups = updateGroups zipped
        finalGroups = groupPointsIter groups 0


-- Function that updates groups iteractively.
-- This is an auxiliary function and should only be called from inside groupPoints
-- Parameters:
--     groups: [Group] -> List of groups in dataset
--     curIter: Int -> Current iteration
-- Result:
--     [Group] -> Resulting group from iteration
groupPointsIter :: [Group] -> Int -> [Group]
groupPointsIter groups curIter
    | curIter == maxIterations = groups
    | not hasChanged = groups
    | otherwise = groupPointsIter newGroups (curIter+1)
    where
        pontos = concat $ map (points) groups
        updatedCentroids = map AP.findCentroid (map (points) groups)
        nearestCentroids = (map (AP.findNearest updatedCentroids) pontos)
        zipped = zip pontos nearestCentroids
        newGroups = updateGroups zipped
        hasChanged = groups /= newGroups


-- Function that groups points together.
-- This is an auxiliary function that should only be called from inside groupPoints
-- Parameters:
--     points: [[(Point, Point)]] -> (Point, Centroid) - Points that must be grouped with their respective centroid
-- Result:
--     [Group] -> Calculated groups
updateGroups :: [(AP.Point, AP.Point)] -> [Group]
updateGroups points = grouped
    where
        dados = groupBy ((==) `on` snd) (sortBy (compare `on` snd) points)
        grouped = map (\tuple -> Group (snd $ head tuple) [fst x | x <- tuple]) dados
