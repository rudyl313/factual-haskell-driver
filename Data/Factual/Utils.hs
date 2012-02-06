module Data.Factual.Utils
  ( join
  , joinAndFilter
  ) where

import Data.List (intersperse)

join :: [a] -> [[a]] -> [a]
join delim xs = concat (intersperse delim xs)

joinAndFilter :: [String] -> String
joinAndFilter strs = join "&" $ filter ("" /=) strs
