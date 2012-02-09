-- | This module exports shared utility methods used by the Factual datatypes.
module Data.Factual.Utils
  (
    -- * Utility methods
    join
  , joinAndFilter
  ) where

import Data.List (intersperse)

-- | The join function joins a list of lists into a list using a separator list.
--   The most common use case is for joining Strings with a common separator
--   String.
join :: [a] -> [[a]] -> [a]
join delim xs = concat (intersperse delim xs)

-- | This function filters out empty Strings from a list before joining the
--   Strings with an & character. The use case is forming query path Strings.
joinAndFilter :: [String] -> String
joinAndFilter strs = join "&" $ filter ("" /=) strs
