-- | This module exports shared utility methods used by the Factual datatypes.
module Data.Factual.Utils
  (
    -- * Utility methods
    join
  , joinAndFilter
  , selectPair
  , limitPair
  , includeCountPair
  ) where

import Data.List (intersperse)
import Network.HTTP.Base (urlEncode)
import qualified Data.Map as M

-- | The join function joins a list of lists into a list using a separator list.
--   The most common use case is for joining Strings with a common separator
--   String.
join :: [a] -> [[a]] -> [a]
join delim xs = concat (intersperse delim xs)

-- | This function filters out empty Strings from a list before joining the
--   Strings with an & character. The use case is forming query path Strings.
joinAndFilter :: [String] -> String
joinAndFilter strs = join "&" $ filter ("" /=) strs

-- The following helper functions are used in generating query Strings.
selectPair :: [String] -> (String, String)
selectPair selects = ("select", join "," selects)

limitPair :: Maybe Int -> (String, String)
limitPair (Just x) = ("limit", show x)
limitPair Nothing  = ("limit", "")

includeCountPair :: Bool -> (String, String)
includeCountPair True  = ("include_count", "true")
includeCountPair False = ("include_count", "false")
