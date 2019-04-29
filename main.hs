import Algebra.Point
import Data.List
import Data.Function


getFirstPoint points = head $ sortBy (compare `on` snd) zipped
    where
        cpoints = map Point points
        zipped = zip (points) (map (coordSum) cpoints)
-- read k.txt
-- read entrada.txt
-- do grouping algorithm
-- write sse value to result.txt
-- write output to saida.txt