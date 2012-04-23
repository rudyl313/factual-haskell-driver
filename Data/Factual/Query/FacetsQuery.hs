-- | This module exports the types used to create facets queries.
module Data.Factual.Query.FacetsQuery
  (
    -- * FacetsQuery type
    FacetsQuery(..)
  ) where

import Data.Factual.Query
import Data.Factual.Table
import Data.Factual.Search
import Data.Factual.Filter
import Data.Factual.Geo
import Data.Factual.Utils

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
  toPath query = (show $ table query)
               ++ "facets?"
               ++ joinAndFilter [ searchString $ search query
                                , selectString $ select query
                                , filtersString $ filters query
                                , geoString $ geo query
                                , limitString $ limit query
                                , minCountString $ minCount query
                                , includeCountString $ includeCount query ]

-- Helper functions
minCountString :: Maybe Int -> String
minCountString (Just x) = "min_count=" ++ show x
minCountString Nothing  = ""
