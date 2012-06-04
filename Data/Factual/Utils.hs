-- | This module exports shared utility methods used by the Factual datatypes.
module Data.Factual.Utils
  (
    -- * Utility methods
    join
  , joinAndFilter
  , selectString
  , limitString
  , includeCountString
  ) where

import Data.List (intersperse)
import Network.HTTP.Base (urlEncode)

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
selectString :: [String] -> String
selectString []      = ""
selectString selects = "select=" ++ (urlEncode $ (join "," selects))

limitString :: Maybe Int -> String
limitString (Just x) = "limit=" ++ (urlEncode $ show x)
limitString Nothing = ""

includeCountString :: Bool -> String
includeCountString True = "include_count=true"
includeCountString False = "include_count=false"
