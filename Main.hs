import Algebra.Point
import Data.List
import Algebra.Group
import System.IO
import Text.Printf

-- Input and output files
__input_file_k = "./testcases/k.txt"
__input_file_points = "./testcases/entrada.txt"
__output_file_groups = "./output/saida.txt"
__output_file_res = "./output/result.txt"

-- Function that formats output for proper display
-- Parameters:
--     groups: [Group] -> List of groups to be displayed
-- Result:
--     [Char] -> String to be outputted 
formatOuput :: [Group] -> [Char]
formatOuput groups = formatted
    where
        ids = reverse $ map (map (identifier)) (map (points) groups)
        sorted = map (sort) ids
        formatted = concat . map (\x -> (concat . intersperse ", " $ (map (show) x)) ++ "\n\n") $ sorted


main = do   putStrLn "Main called"
            readK <- readFile __input_file_k
            readP <- readFile __input_file_points
            let dataset = map (map (read::String->Double)) (map (words) (lines readP))
            let k = read readK :: Int
            let centroids = findCentroidsFromDataset dataset k
            let finalGroups = groupPoints (convertDataset dataset) centroids
            writeFile __output_file_res $ printf "%.4f" (getTotalSSE finalGroups)
            writeFile __output_file_groups $ formatOuput $ finalGroups
            return()