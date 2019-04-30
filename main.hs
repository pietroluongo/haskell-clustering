import Algebra.Point
import Data.List
import Data.Function

-- Function that finds the first point of the dataset
-- Parameters:
--      points: [[Int]], the dataset where each element on the list represents one point on the dataset
-- Output:
--      point: The point that should be the first one to be evaluated
-- The next function to be called should be getSecondPoint, passing the dataset and the output of this function as parameters
getFirstPoint points = head minPoint
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
getSecondPoint points initial = maxDistPoint
    where
        cpoints = map Point points
        cpoint = Point initial
        zipped = zip (points) (map (dist cpoint) cpoints)
        sorted = drop 1 $ sortBy (compare `on` snd) zipped
        maxDist = snd $ last sorted
        pointsWithMaxDist = [x | x <- sorted, (snd x) == maxDist]
        maxDistPoint = sortBy (compare `on` fst) pointsWithMaxDist 
-- read k.txt
-- read entrada.txt
-- do grouping algorithm
-- write sse value to result.txt
-- write output to saida.txt