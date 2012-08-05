-- | This module exports the types used to create read queries.
module Data.Factual.Query.ReadQuery
  (
    -- * ReadQuery type
    ReadQuery(..)
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
  path query   = show $ table query
  params query = M.fromList [ searchPair       $ search query
                            , selectPair       $ select query
                            , limitPair        $ limit query
                            , offsetPair       $ offset query
                            , filtersPair      $ filters query
                            , geoPair          $ geo query
                            , includeCountPair $ includeCount query ]

-- The following helper functions are used in generating query Strings.
offsetPair :: Maybe Int -> (String, String)
offsetPair (Just x) = ("offset", show x)
offsetPair Nothing  = ("offset", "")
