import Algebra.Point
import Data.List
import Data.Function
import Utils
import Algebra.Group
import System.IO
-- DEBUGGING ---------------------------------------------

__dataset = [[3.0, 2.0, 1.0], [1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0], [9.0, 8.0, 7.0]]

--__dataset_fst_point = getFirstPoint __dataset

__input_file_k = "./testcases/k.txt"
__input_file_points = "./testcases/entrada.txt"
__output_file_groups = "./output/saida.txt"
__output_file_res = "./output/result.txt"

__CONST_MAX_IT = 100

----------------------------------------------------------

-- read k.txt
-- read entrada.txt
-- do grouping algorithm
-- write sse value to result.txt
-- write output to saida.txt

main = do   putStrLn "Main called"
            readK <- readFile __input_file_k
            readP <- readFile __input_file_points
            let dataset = map (read :: Double) (lines readP)
            let d = findCentroidsFromDataset dataset (read readK :: Int)
            writeFile __output_file_res "nil"
            writeFile __output_file_groups "nil"
            return()