module Algebra.Point (
    Point(..),
    dist,
    centroid,
    sse,
    coordSum,
    getPoints
) where

import Data.List
import Utils
import Data.Function

-- Abstract type "Point"
-- Fields:
--      coords: [Float] -> List of coordinates. Element i represents the coordinate of the point on the i-th dimension.
data Point = Point {
    coords :: [Float]
} deriving (Show)

-- Function that calculates the euclidean distance between two points.
-- Parameters:
--      p1: Point -> First point
--      p2: Point -> Second point
-- Result:
--      Float -> Euclidean distance between p1 and p2
dist :: Point -> Point -> Float
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
--      Float -> Calculated SSE
sse :: [Point] -> Point -> Float
sse points centroid = foldl (\acc x -> acc + (dist x centroid)^2) 0 points

-- Function that finds the sum of the coordinates of a point.
-- Parameters:
--      point: Point -> Point to be analyzed
-- Result:
--      Float -> Sum of the coordinates of the point
coordSum :: Point -> Float
coordSum point = sum $ coords point

-------------------------------------------------------------------------------------------------------------------------------------------
-- Project specific stuff
-------------------------------------------------------------------------------------------------------------------------------------------

-- Function that finds the first point of the dataset.
-- This is an auxiliary function, and should be called exclusively from inside getPoints.
-- Parameters:
--      points: [[Float]] -> The dataset, with each element on the list representing one point on the dataset
-- Result:
--      [Float] -> The coordinates of the point that should be the first one to be evaluated following the project specification
-- The next function to be called should be getSecondPoint, with the dataset and the output of this function as parameters.
getFirstPoint :: [[Float]] -> [Float]
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
--      points: [[Float]] -> The dataset, with each element on the list representing one point on the dataset
--      initial: [Float] -> The initial point of the dataset (calculated previously on getFirstPoint)
-- Result:
--      [Float] -> The coordinates of the point that should be the second one to be evaluated following the project specification
getSecondPoint :: [[Float]] -> [Float] -> [Float]
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
--      points: [[Float]] -> Points to be analyzed
--      k: Num -> Number of centroids to be chosen
-- Result:
--      [[Float]] -> Centroids from groups
getCentroids :: (Eq a, Num a) => [[Float]] -> a -> [[Float]]
getCentroids points k
    | k == 0 = []
    | otherwise = (coords $ fst closest):getCentroids (remove points $ coords $ fst closest) (k-1)
    where
        pPoints = map Point points
        cent = centroid pPoints
        dists = map (dist cent) pPoints
        sorted = sortBy (compare `on` snd) $ zip pPoints dists
        closest = head sorted

-- Function that sets K points as the centroids of the groups. This is the main grouping function.
-- Parameters:
--     dataset: [[Float]] -> Points to be analyzed
--     k: Num -> Number of centroids to be chosen
-- Result:
--     [[Float]] -> List containing K centroids for K groups
getPoints :: (Eq a, Num a) => [[Float]] -> a -> [[Float]]
getPoints dataset k
    | k == 0 = []
    | otherwise = firstPoint:secondPoint:getCentroids filteredDataset (k-2)
    where
        firstPoint = getFirstPoint dataset
        secondPoint = getSecondPoint dataset firstPoint
        filteredDataset' = remove dataset firstPoint
        filteredDataset = remove filteredDataset' secondPoint

