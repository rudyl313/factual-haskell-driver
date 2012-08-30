-- | This module exports the SortOrder type used to create read and facet queries.
module Data.Factual.Shared.SortOrder
  (
    -- * SortOrder type
    SortOrder(..)
    -- * Helper functions
  , sortPair
  ) where

import Data.List.Utils (join)

-- | The SortOrder type is used to represent sorting parameters
data SortOrder = Asc String | Desc String deriving Eq

-- SortOrder is a member of Show to help generate query strings.
instance Show SortOrder where
  show (Asc field)  = field ++ ":asc"
  show (Desc field) = field ++ ":desc"

-- The following helper function is used in generating query params.
sortPair :: [SortOrder] -> (String, String)
sortPair []    = ("sort", "")
sortPair sorts = ("sort", join "," $ map show sorts)
