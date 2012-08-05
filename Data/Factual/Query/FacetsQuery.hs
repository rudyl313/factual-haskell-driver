-- | This module exports the types used to create facets queries.
module Data.Factual.Query.FacetsQuery
  (
    -- * FacetsQuery type
    FacetsQuery(..)
    -- * Required modules
  , module Data.Factual.Shared.Table
  , module Data.Factual.Shared.Search
  , module Data.Factual.Shared.Filter
  , module Data.Factual.Shared.Geo
  ) where

import Data.Factual.Query
import Data.Factual.Shared.Table
import Data.Factual.Shared.Search
import Data.Factual.Shared.Filter
import Data.Factual.Shared.Geo
import Data.Factual.Utils
import qualified Data.Map as M

-- | The FacetsQuery type is used to construct facets queries. A table and search
--   should be specified, but the rest of the query options are essentially
--   optional.
data FacetsQuery = FacetsQuery { table        :: Table
                               , search       :: Search
                               , select       :: [String]
                               , filters      :: [Filter]
                               , geo          :: Maybe Geo
                               , limit        :: Maybe Int
                               , minCount     :: Maybe Int
                               , includeCount :: Bool
                               } deriving (Eq, Show)

-- The FacetsQuery type is a member of the Query typeclass so it can be used to
-- make a request.
instance Query FacetsQuery where
  path query   = (show $ table query) ++ "/facets"
  params query = M.fromList [ searchPair $ search query
                            , selectPair $ select query
                            , filtersPair $ filters query
                            , geoPair $ geo query
                            , limitPair $ limit query
                            , minCountPair $ minCount query
                            , includeCountPair $ includeCount query ]

-- Helper functions
minCountPair :: Maybe Int -> (String, String)
minCountPair (Just x) = ("min_count", show x)
minCountPair Nothing  = ("min_count", "")
