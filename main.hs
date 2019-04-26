import Algebra.Point
import Data.List
stuffs _ [] = []
stuffs [] _ = []
stuffs (x:xs) (y:ys) = [x+y]:stuffs xs ys

--stuffsDim :: (Num a) => [[a]] -> [a]
stuffsDim xss = foldl (\acc xs -> acc + sum xs) 0 txss
    where
        txss = transpose xss
