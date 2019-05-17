module Algebra.Point (
    Point(..),
    dist,
    findCentroid,
    sse,
    coordSum,
    findCentroidsFromDataset,
    findNearest,
    filterDataset,
    convertDataset
) where

import Data.List
import Utils
import Data.Function

-- Abstract type "Point"
-- Fields:
--      coords: [Double] -> List of coordinates. Element i represents the coordinate of the point on the i-th dimension.
data Point = Point {
    coords     :: [Double],
    identifier :: Int
} deriving (Show, Eq, Ord)

-- Function that calculates the euclidean distance between two points.
-- Parameters:
--      p1: Point -> First point
--      p2: Point -> Second point
-- Result:
--      Double -> Euclidean distance between p1 and p2
dist :: Point -> Point -> Double
dist p1 p2 = sqrt sumOfSquares
    where
        sumOfSquares = sum $ zipWith (\x1 x2 -> (x1-x2)^2) (coords p1) (coords p2)

-- Function to find the centroid of a list of points.
-- Parameters:
--      points: [Point] -> List of points to find the centroid from
-- Result:
--      Point-> Point that is the centroid of the group
findCentroid :: [Point] -> Point
findCentroid points = Point centroidCoords (-1)
    where
        pointLength = fromIntegral (length(points))
        pointList = map coords points
        transposedPointList = transpose pointList
        colSum = map sum transposedPointList
        centroidCoords = map (/pointLength) colSum

-- Function that finds the sum of the squared euclidean distances (SSE).
-- Parameters: 
--      points: [Point] -> List of points to find the SSE from
--      centroid: Point -> Centroid from the group
-- Result:
--      Double -> Calculated SSE
sse :: [Point] -> Point -> Double
sse points centroid = foldl (\acc x -> acc + (dist x centroid)^2) 0 points

-- Function that finds the sum of the coordinates of a point.
-- Parameters:
--      point: Point -> Point to be analyzed
-- Result:
--      Double -> Sum of the coordinates of the point
coordSum :: Point -> Double
coordSum point = sum $ coords point


-- Finds the point in a group that is closest to another point
-- Parameters:
--     group: [Point] -> List of points to be analyzed
--     point: Point -> Point to be grouped
-- Result:
--     Point -> Point in group with the smallest euclidean distance from the input point
findNearest :: [Point] -> Point -> Point
findNearest group point = minPoint
    where
        dists = map (dist point) group
        zipped = zip group dists
        sorted = sortBy (compare `on` snd) zipped
        minDist = snd $ head sorted
        smol = [x | x <- sorted, (snd x) == minDist]
        minPoint = fst $ head $ sortBy (compare `on` fst) smol

filterDataset :: [Point] -> [Point] -> [Point]
filterDataset pointsToBeRemoved allPoints = [x | x <- allPoints, not $ x `elem` pointsToBeRemoved]

convertDataset :: [[Double]] -> [Point]
convertDataset dataset = [Point (fst y) (snd y) | y <- (zip dataset [1..])]

-------------------------------------------------------------------------------------------------------------------------------------------
-- Project specific stuff
-------------------------------------------------------------------------------------------------------------------------------------------

-- Function that finds the first point of the dataset.
-- This is an auxiliary function, and should be called exclusively from inside getPoints.
-- Parameters:
--      points: [Points] -> The dataset, with each element on the list representing one point on the dataset
-- Result:
--      Point -> The coordinates of the point that should be the first one to be evaluated following the project specification
-- The next function to be called should be getSecondPoint, with the dataset and the output of this function as parameters.
getFirstPoint :: [Point] -> Point
getFirstPoint points = minPoint
    where
        zipped = zip (points) (map (coordSum) points)
        sorted = sortBy (compare `on` snd) zipped
        minSum = snd $ head sorted
        pointsWithMinSum = [x | x <- sorted, (snd x) == minSum]
        minPoint = fst $ head $ sortBy (compare `on` fst) pointsWithMinSum

-- Function that finds the second point of the dataset.
-- This is an auxiliary function, and should be called exclusively from inside getPoints.
-- Parameters:
--      points: [Point] -> The dataset, with each element on the list representing one point on the dataset
--      initial: Point -> The initial point of the dataset (calculated previously on getFirstPoint)
-- Result:
--      [Point] -> The coordinates of the point that should be the second one to be evaluated following the project specification
getSecondPoint :: [Point] -> Point -> Point
getSecondPoint points initial = maxDistPoint
    where
        zipped = zip (points) (map (dist initial) points)
        sorted = drop 1 $ sortBy (compare `on` snd) zipped
        maxDist = snd $ last sorted
        pointsWithMaxDist = [x | x <- sorted, (snd x) == maxDist]
        maxDistPoint = fst $ head $ sortBy (compare `on` fst) pointsWithMaxDist

-- Function that sets K points as the centroids of the groups, but without considering the first and second points specifically.
-- This is an auxiliary function, and should be called exclusively from inside getPoints.
-- Parameters:
--      points: [Point] -> Points to be analyzed
--      k: Num -> Number of centroids to be chosen
--      selected: [Point] -> Points chosen previously
-- Result:
--      [Point] -> Centroids from groups
getCentroids :: (Eq a, Num a) => [Point] -> a -> [Point] -> [Point]
getCentroids points k selected
    | k == 0 = []
    | otherwise = furthest:getCentroids (remove points furthest) (k-1) (furthest:selected)
    where
        cent = findCentroid selected
        dists = map (dist cent) points
        sorted = sortBy (compare `on` snd) $ zip points dists
        furthest = fst $ last sorted

-- Function that sets K points as the centroids of the groups. This is the main grouping function.
-- Parameters:
--     dataset: [[Double]] -> Points to be analyzed
--     k: Num -> Number of centroids to be chosen
-- Result:
--     [Point] -> List containing K centroids for K groups
findCentroidsFromDataset :: [[Double]] -> Int -> [Point]
findCentroidsFromDataset dataset k
    | k > length (nub dataset) = []
    | k == 0 = []
    | k == 1 = [firstPoint]
    | k == 2 = firstPoint:[secondPoint]
    | otherwise = firstPoint:secondPoint:getCentroids filteredDataset (k-2) (firstPoint:[secondPoint])
    where
        z = zip dataset ([1..])
        _dataset = [Point (fst y) (snd y) | y <- z]
        firstPoint = getFirstPoint _dataset
        secondPoint = getSecondPoint _dataset firstPoint
        filteredDataset' = remove _dataset firstPoint
        filteredDataset = remove filteredDataset' secondPoint
