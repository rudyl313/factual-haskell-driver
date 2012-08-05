-- | This module exports the definition of the Query typeclass.
module Data.Factual.Query
  (
  -- * Query typeclass
  Query(..)) where

import qualified Data.Map as M

-- | A member of the Query typeclass must define a path function which outputs
--   the Query endpoint path, and a params function that outputs a Map of query
--   params keys and values.
class Query q where
  path   :: q -> String
  params :: q -> M.Map String String
