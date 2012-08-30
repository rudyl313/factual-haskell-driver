-- | This module exports shared utility methods used by the Factual datatypes.
module Data.Factual.Utils
  (
    -- * Utility methods
    selectPair
  , limitPair
  , includeCountPair
  ) where

import Data.List (intersperse)
import qualified Data.Map as M
import Data.List.Utils (join)

-- The following helper functions are used in generating query params Maps.
selectPair :: [String] -> (String, String)
selectPair selects = ("select", join "," selects)

limitPair :: Maybe Int -> (String, String)
limitPair (Just x) = ("limit", show x)
limitPair Nothing  = ("limit", "")

includeCountPair :: Bool -> (String, String)
includeCountPair True  = ("include_count", "true")
includeCountPair False = ("include_count", "false")
