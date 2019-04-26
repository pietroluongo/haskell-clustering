import Algebra.Point
import Data.List
stuffs _ [] = []
stuffs [] _ = []
stuffs (x:xs) (y:ys) = [x+y]:stuffs xs ys

--stuffsDim :: (Num a) => [[a]] -> [a]
stuffsDim xss = map (sum) txss
    where
        txss = transpose xss