-- | This module exports the type used to create geopulse queries.
module Data.Factual.Query.GeocodeQuery
  (
    -- * ResolveQuery type
    GeocodeQuery(..)
    -- * Required modules
  , module Data.Factual.Shared.Geo
  ) where

import Data.Factual.Query
import Data.Factual.Shared.Geo
import qualified Data.Map as M

-- | The GeocodeQuery type is used to construct geocode queries. A geo point
--   is required.
data GeocodeQuery = GeocodeQuery Geo deriving (Eq, Show)

-- The GeocodeQuery type is a member of the Query typeclass so it can be used
-- to make a request.
instance Query GeocodeQuery where
  path   _                  = "/places/geocode"
  params (GeocodeQuery geo) = M.fromList [(geoPair $ Just geo)]
