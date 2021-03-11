module Toolkits.Common where 

import Data.List (group, sort)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
