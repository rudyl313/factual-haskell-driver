-- | This module exports the types used to create read queries.
module Data.Factual.Query.ReadQuery
  (
    -- * ReadQuery type
    ReadQuery(..)
  ) where

import Data.Factual.Query
import Data.Factual.Shared.Table
import Data.Factual.Shared.Search
import Data.Factual.Shared.Filter
import Data.Factual.Shared.Geo
import Data.Factual.Utils

-- | The ReadQuery type is used to construct read queries. A table should be
--   specified, but the rest of the query options are essentially optional
--   (you opt out using Nothing or an empty List for the value). The select is
--   a list of field names to include in the results. The limit and offset are
--   used to request a specific range of rows and includeCount will include the
--   count of returned rows if it is set to True.
data ReadQuery = ReadQuery { table        :: Table
                           , search       :: Search
                           , select       :: [String]
                           , limit        :: Maybe Int
                           , offset       :: Maybe Int
                           , filters      :: [Filter]
                           , geo          :: Maybe Geo
                           , includeCount :: Bool
                           } deriving (Eq, Show)

-- The ReadQuery type is a member of the Query typeclass so it can be used to
-- make a request.
instance Query ReadQuery where
  toPath query = (show $ table query)
               ++ "read?"
               ++ joinAndFilter [ searchString $ search query
                                , selectString $ select query
                                , limitString $ limit query
                                , offsetString $ offset query
                                , filtersString $ filters query
                                , geoString $ geo query
                                , includeCountString $ includeCount query ]

-- The following helper functions are used in generating query Strings.
offsetString :: Maybe Int -> String
offsetString (Just x) = "offset=" ++ show x
offsetString Nothing = ""
