-- | This module exports the type used to create geopulse queries.
module Data.Factual.Query.GeopulseQuery
  (
    -- * ResolveQuery type
    GeopulseQuery(..)
    -- * Required modules
  , module Data.Factual.Shared.Geo
  ) where

import Data.Factual.Query
import Data.Factual.Utils
import Data.Factual.Shared.Geo
import qualified Data.Map as M

-- | The GeopulseQuery type is used to construct geopulse queries. A geo point
--   is required but select values are optional (just use an empty list to
--   denote selecting all pulses).
data GeopulseQuery = GeopulseQuery { geo    :: Geo
                                   , select :: [String]
                                   } deriving (Eq, Show)

-- The GeopulseQuery type is a member of the Query typeclass so it can be used
-- to make a request.
instance Query GeopulseQuery where
  path   _     = "/places/geopulse"
  params query = M.fromList [ geoPair $ Just $ geo query
                            , selectPair $ select query ]
