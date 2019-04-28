module Algebra.Point (
    Point(..),
    dist,
    centroid,
    sse
) where

import Data.List

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
