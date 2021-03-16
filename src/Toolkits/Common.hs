module Toolkits.Common where 

import Data.List (group, sort)

rmdups :: Eq a => [a] -> [a]
rmdups = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs
