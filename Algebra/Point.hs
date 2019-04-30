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
--      coords: List of coordinates. Element i means the coordinate of the point on the i-th dimension
data Point = Point {
    coords :: [Float]
} deriving (Show)

-- Function to calculate the euclidean distance between two points
-- Parameters:
--      p1: First point
--      p2: Second point
-- Result:
--      Float: Euclidean distance between p1 and p2
dist :: Point -> Point -> Float
dist p1 p2 = sqrt sumOfSquares
    where
        sumOfSquares = sum $ zipWith (\x1 x2 -> (x1-x2)^2) (coords p1) (coords p2)

-- Function to find the centroid of a list of points
-- Parameters:
--      points: List of points to find the centroid from
-- Result:
--      Point: Point that is the centroid of the group
centroid :: [Point] -> Point
centroid points = Point centroidCoords
    where
        pointLength = fromIntegral (length(points))
        pointList = map coords points
        transposedPointList = transpose pointList
        colSum = map sum transposedPointList
        centroidCoords = map (/pointLength) colSum

-- Function that finds the sum of the squared euclidean distances (SSE)
-- Parameters: 
--      points: List of points to find the SSE from
--      centroid: Centroid from the group
-- Result:
--      Float: Calculated SSE
sse :: [Point] -> Point -> Float
sse points centroid = foldl (\acc x -> acc + (dist x centroid)^2) 0 points

-- Function that finds the sum of the coordinates of a point
-- Parameters:
--      point: Point to be analyzed
-- Result:
--      Float: Sum of the coordinates of the point
coordSum :: Point -> Float
coordSum point = sum $ coords point

-------------------------------------------------------------------------------------------------------------------------------------------
-- Project specific stuff
-------------------------------------------------------------------------------------------------------------------------------------------

-- Function that finds the first point of the dataset
-- Parameters:
--      points: [[Int]], the dataset where each element on the list represents one point on the dataset
-- Output:
--      point: The point that should be the first one to be evaluated
-- The next function to be called should be getSecondPoint, passing the dataset and the output of this function as parameters
getFirstPoint points = fst $ head minPoint
    where
        cpoints = map Point points
        zipped = zip (points) (map (coordSum) cpoints)
        sorted = sortBy (compare `on` snd) zipped
        minSum = snd $ head sorted
        pointsWithMinSum = [x | x <- sorted, (snd x) == minSum]
        minPoint = sortBy (compare `on` fst) pointsWithMinSum


-- Function that finds the second point of the dataset
-- Parameters:
--      points: [[Int]], the dataset where each element on the list represents one point on the dataset
--      initial: [Int], the initial point of the dataset (calculated on getFirstPoint)
-- Output:
--      point: The second point of the Dataset
getSecondPoint points initial = fst $ head maxDistPoint
    where
        cpoints = map Point points
        cpoint = Point initial
        zipped = zip (points) (map (dist cpoint) cpoints)
        sorted = drop 1 $ sortBy (compare `on` snd) zipped
        maxDist = snd $ last sorted
        pointsWithMaxDist = [x | x <- sorted, (snd x) == maxDist]
        maxDistPoint = sortBy (compare `on` fst) pointsWithMaxDist

-- Function that sets K points as the centroids of the groups, but without considering the first and second points specifically. Called by getPoints
-- Parameters:
--      points: [[Int]] points to be analyzed
--      k: Int, # of points to be picked
-- Output:
--      [Points]: Centroids from groups
getCentroids points k
    | k == 0 = []
    | otherwise = (coords $ fst closest):getCentroids (remove points $ coords $ fst closest) (k-1)
    where
        pPoints = map Point points
        cent = centroid pPoints
        dists = map (dist cent) pPoints
        sorted = sortBy (compare `on` snd) $ zip pPoints dists
        closest = head sorted

-- Function that sets K points as the centroids of the groups. This is the main grouping function
-- Parameters:
--     dataset: [[Int]] points to be analyzed
--     k: Int, # of points
-- Output:
--     [Points]: Centroids from all groups 
getPoints dataset k
    | k == 0 = []
    | otherwise = firstPoint:secondPoint:getCentroids filteredDataset (k-2)
    where
        firstPoint = getFirstPoint dataset
        secondPoint = getSecondPoint dataset firstPoint
        filteredDataset' = remove dataset firstPoint
        filteredDataset = remove filteredDataset' secondPoint

