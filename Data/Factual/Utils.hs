-- | This module exports shared utility methods used by the Factual datatypes.
module Data.Factual.Utils
  (
    -- * Utility methods
    join
  , selectPair
  , limitPair
  , includeCountPair
  ) where

import Data.List (intersperse)
import qualified Data.Map as M

-- | The join function joins a list of lists into a list using a separator list.
--   The most common use case is for joining Strings with a common separator
--   String.
join :: [a] -> [[a]] -> [a]
join delim xs = concat (intersperse delim xs)

-- The following helper functions are used in generating query params Maps.
selectPair :: [String] -> (String, String)
selectPair selects = ("select", join "," selects)

limitPair :: Maybe Int -> (String, String)
limitPair (Just x) = ("limit", show x)
limitPair Nothing  = ("limit", "")

includeCountPair :: Bool -> (String, String)
includeCountPair True  = ("include_count", "true")
includeCountPair False = ("include_count", "false")
