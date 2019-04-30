module Utils (
    remove
) where

--remove :: [a] -> [a]
remove list element = filter (\x -> x/=element) list