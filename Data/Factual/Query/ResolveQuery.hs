-- | This module exports the type used to create resolve queries.
module Data.Factual.Query.ResolveQuery
  (
    -- * ResolveQuery type
    ResolveQuery(..)
    -- * ResolveValue type
  , ResolveValue(..)
  ) where

import Data.Factual.Query
import Data.Factual.Utils
import qualified Data.Map as M

-- | A resolve value can either be a String or a Number (Double). The first
--   argument is the name of the field and the second argument is the input
--   value.
data ResolveValue = ResolveStr String String
                  | ResolveNum String Double
                  deriving Eq

-- ResolveValue is a member of the Show typeclass to help generate query Strings.
instance Show ResolveValue where
  show (ResolveStr name str) = (show name) ++ ":" ++ (show str)
  show (ResolveNum name num) = (show name) ++ ":" ++ (show num)

-- | A resolve query is formed as an array of resolve values. These values will
--   be compared with Factual records to return a cleaner, more canonical row
--   of data.
data ResolveQuery = ResolveQuery { values :: [ResolveValue]
                                 , debug  :: Bool
                                 } deriving Eq

-- ResolveQuery is a member of the Query typeclass so that it can be used to
-- make requests.
instance Query ResolveQuery where
  path   _     = "/places/resolve"
  params query = M.fromList [("values", valuesString), ("debug", debugString)]
    where valuesString = "{" ++ (join "," $ map show $ values query) ++ "}"
          debugString  = if (debug query) then "true" else "false"
