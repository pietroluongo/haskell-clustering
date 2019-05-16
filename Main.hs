import Algebra.Point
import Data.List
import Data.Function
import Utils
import Algebra.Group
import System.IO
-- DEBUGGING ---------------------------------------------

__dataset = [[3.0, 2.0, 1.0], [1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0], [9.0, 8.0, 7.0]]

__dataset_conv = [Point (fst y) (snd y) | y <- (zip __dataset [1..])]

--__dataset_fst_point = getFirstPoint __dataset

__input_file_k = "./testcases/k.txt"
__input_file_points = "./testcases/entrada.txt"
__output_file_groups = "./output/saida.txt"
__output_file_res = "./output/result.txt"

__CONST_MAX_IT = 100

__test = "7 5.4 6.32 9\n17 32.3 5 9.99\n33 54 5.6 65.8\n77.7 33.4 98 7.56\n8.9 5.8 6 9"

----------------------------------------------------------

-- read k.txt
-- read entrada.txt
-- do grouping algorithm
-- write sse value to result.txt
-- write output to saida.txt


_dbg = groupStuff __dataset_conv c
    where
        c = findCentroidsFromDataset __dataset 3
        -- p = filterDataset c __dataset_conv


groupStuff points centroids = groups
    where
        zipped = zip points (map (findNearest centroids) points)
        groups = groupPoints zipped

group_iter = 0

main = do   putStrLn "Main called"
            readK <- readFile __input_file_k
            readP <- readFile __input_file_points
            let dataset = map (map (read::String->Double)) (map (words) (lines readP))
            let k = read readK :: Int
            let d = findCentroidsFromDataset dataset k
            putStrLn $ show d
            writeFile __output_file_res $ "nil"
            writeFile __output_file_groups "nil"
            return()