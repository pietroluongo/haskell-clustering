import Algebra.Point
import Data.List
import Algebra.Group
import System.IO
import Text.Printf

-- Input and output files
inputFileK = "./k.txt"
inputFilePoints = "./entrada.txt"
outputFileGroups = "./saida.txt"
outputFileRes = "./result.txt"

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
            readK <- readFile inputFileK
            readP <- readFile inputFilePoints
            let dataset = map (map (read::String->Double)) (map (words) (lines readP))
            let k = read readK :: Int
            let centroids = findCentroidsFromDataset dataset k
            let finalGroups = groupPoints (convertDataset dataset) centroids
            writeFile outputFileRes $ printf "%.4f" (getTotalSSE finalGroups)
            writeFile outputFileGroups $ formatOuput $ finalGroups
            return()