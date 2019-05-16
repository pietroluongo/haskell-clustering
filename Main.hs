import Algebra.Point
import Data.List
import Data.Function
import Utils
import Algebra.Group
import System.IO
import Text.Printf
-- DEBUGGING ---------------------------------------------

__dataset = [[3.0, 2.0, 1.0], [1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0], [9.0, 8.0, 7.0]]

__dataset_conv = convertDataset __dataset

__input_file_k = "./testcases/k.txt"
__input_file_points = "./testcases/entrada.txt"
__output_file_groups = "./output/saida.txt"
__output_file_res = "./output/result.txt"

__CONST_MAX_IT = 100

__test = [[7.0, 5.4, 6.32, 9.0], [17.0, 32.3, 5.0, 9.99], [33.0, 54.0, 5.6, 65.8], [77.7, 33.4, 98.0, 7.56], [8.9, 5.8, 6.0, 9.0]]

__test_conv = convertDataset __test

----------------------------------------------------------

-- read k.txt
-- read entrada.txt
-- do grouping algorithm
-- write sse value to result.txt
-- write output to saida.txt


_dbg = d
    where
        a = findCentroidsFromDataset __test 3
        b = groupStuff __test_conv a
        c = group_iter b 0
        d = getTotalSSE c

groupStuff points centroids = groups
    where
        zipped = zip points (map (findNearest centroids) points)
        groups = groupPoints zipped

group_iter groups cur_iter
    | cur_iter == __CONST_MAX_IT = groups
    | not hasChanged = groups
    | otherwise = newGroups--group_iter newGroups (cur_iter+1)
    where
        pontos = concat $ map (points) groups
        updatedCentroids = recalculateCentroids groups
        nearestCentroids = (map (findNearest updatedCentroids) pontos)
        zipped = zip pontos nearestCentroids
        newGroups = groupPoints zipped
        hasChanged = groups /= newGroups

recalculateCentroids groups = b
    where
        b = map findCentroid (map (points) groups)

getOutStrings groups = asdf
    where
        asdf = map (map (identifier)) (map (points) groups)
        --out = getOutStringsRec asdf



main = do   putStrLn "Main called"
            readK <- readFile __input_file_k
            readP <- readFile __input_file_points
            let dataset = map (map (read::String->Double)) (map (words) (lines readP))
            let k = read readK :: Int
            let centroids = findCentroidsFromDataset dataset k
            let conv = convertDataset dataset
            let groupedStuff = groupStuff conv centroids
            let ree = group_iter groupedStuff 0
            let roo = getOutStrings ree
            let sse = getTotalSSE ree
            writeFile __output_file_res $ printf "%.4f" sse
            writeFile __output_file_groups (show roo)
            return()