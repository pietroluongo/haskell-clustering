module Algebra.Point (
    Point(..),
    dist,
    centroid,
    sse,
    coordSum,
    getPoints,
    findNearest
) where

import Data.List
import Utils
import Data.Function

-- Abstract type "Point"
-- Fields:
--      coords: [Double] -> List of coordinates. Element i represents the coordinate of the point on the i-th dimension.
data Point = Point {
    coords :: [Double]
} deriving (Show)

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
centroid :: [Point] -> Point
centroid points = Point centroidCoords
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
--     point: Point -> Point to be grouped
--     group: [Point] -> List of points to be analyzed
-- Result:
--     Point -> Point in group with the smallest euclidean distance from the input point
findNearest :: Point -> [Point] -> Point
findNearest point group = fst closest
    where
        dists = map (dist point) group
        zipped = zip group dists
        closest = head $ sortBy (compare `on` snd) zipped



-------------------------------------------------------------------------------------------------------------------------------------------
-- Project specific stuff
-------------------------------------------------------------------------------------------------------------------------------------------

-- Function that finds the first point of the dataset.
-- This is an auxiliary function, and should be called exclusively from inside getPoints.
-- Parameters:
--      points: [[Double]] -> The dataset, with each element on the list representing one point on the dataset
-- Result:
--      [Double] -> The coordinates of the point that should be the first one to be evaluated following the project specification
-- The next function to be called should be getSecondPoint, with the dataset and the output of this function as parameters.
getFirstPoint :: [[Double]] -> [Double]
getFirstPoint points = fst $ head minPoint
    where
        cpoints = map Point points
        zipped = zip (points) (map (coordSum) cpoints)
        sorted = sortBy (compare `on` snd) zipped
        minSum = snd $ head sorted
        pointsWithMinSum = [x | x <- sorted, (snd x) == minSum]
        minPoint = sortBy (compare `on` fst) pointsWithMinSum


-- Function that finds the second point of the dataset.
-- This is an auxiliary function, and should be called exclusively from inside getPoints.
-- Parameters:
--      points: [[Double]] -> The dataset, with each element on the list representing one point on the dataset
--      initial: [Double] -> The initial point of the dataset (calculated previously on getFirstPoint)
-- Result:
--      [Double] -> The coordinates of the point that should be the second one to be evaluated following the project specification
getSecondPoint :: [[Double]] -> [Double] -> [Double]
getSecondPoint points initial = fst $ head maxDistPoint
    where
        cpoints = map Point points
        cpoint = Point initial
        zipped = zip (points) (map (dist cpoint) cpoints)
        sorted = drop 1 $ sortBy (compare `on` snd) zipped
        maxDist = snd $ last sorted
        pointsWithMaxDist = [x | x <- sorted, (snd x) == maxDist]
        maxDistPoint = sortBy (compare `on` fst) pointsWithMaxDist

-- Function that sets K points as the centroids of the groups, but without considering the first and second points specifically.
-- This is an auxiliary function, and should be called exclusively from inside getPoints.
-- Parameters:
--      points: [[Double]] -> Points to be analyzed
--      k: Num -> Number of centroids to be chosen
-- Result:
--      [[Double]] -> Centroids from groups
getCentroids :: (Eq a, Num a) => [[Double]] -> a -> [[Double]]
getCentroids points k
    | k == 0 = []
    | otherwise = (coords $ fst furthest):getCentroids (remove points $ coords $ fst furthest) (k-1)
    where
        pPoints = map Point points
        cent = centroid pPoints
        dists = map (dist cent) pPoints
        sorted = sortBy (compare `on` snd) $ zip pPoints dists
        furthest = last sorted

-- Function that sets K points as the centroids of the groups. This is the main grouping function.
-- Parameters:
--     dataset: [[Double]] -> Points to be analyzed
--     k: Num -> Number of centroids to be chosen
-- Result:
--     [[Double]] -> List containing K centroids for K groups
getPoints :: [[Double]] -> Int -> [[Double]]
getPoints dataset k
    | k > length (nub dataset) = []
    | k == 0 = []
    | k == 1 = [firstPoint]
    | k == 2 = firstPoint:[secondPoint]
    | otherwise = firstPoint:secondPoint:getCentroids filteredDataset (k-2)
    where
        firstPoint = getFirstPoint dataset
        secondPoint = getSecondPoint dataset firstPoint
        filteredDataset' = remove dataset firstPoint
        filteredDataset = remove filteredDataset' secondPoint

