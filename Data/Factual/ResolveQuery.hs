-- | This module exports the type used to create resolve queries
module Data.Factual.ResolveQuery
  (
    -- * ResolveQuery type
    ResolveQuery(..)
    -- * ResolveValue type
  , ResolveValue(..)
  ) where

import Data.Factual.Query
import Data.Factual.Utils

-- | A resolve value can either be a String or a Number (Double). The first
--   argument is the name of the field and the second argument is the input
--   value.
data ResolveValue = ResolveStr String String
                  | ResolveNum String Double
                  deriving Eq

instance Show ResolveValue where
  show (ResolveStr name str) = (show name) ++ ":" ++ (show str)
  show (ResolveNum name num) = (show name) ++ ":" ++ (show num)

-- | A resolve query is formed as an array of resolve values. These values will
--   be compared with Factual records to return a cleaner, more canonical row
--   of data.
data ResolveQuery = ResolveQuery [ResolveValue] deriving Eq

instance Query ResolveQuery where
  toPath (ResolveQuery values) = "/places/resolve?values={"
                               ++ (join "," $ map show values)
                               ++ "}"
