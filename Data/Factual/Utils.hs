module Data.Factual.Utils (join) where

import Data.List (intersperse)

join :: [a] -> [[a]] -> [a]
join delim xs = concat (intersperse delim xs)
