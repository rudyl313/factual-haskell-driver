-- | This module exports the definition of the Query typeclass.
module Data.Factual.Query
  (
  -- * Query typeclass
  Query(..)) where

import qualified Data.Map as M

-- | A member of the Query typeclass must define a toPath method which converts
--   the Query into a path String.
class Query q where
  path   :: q -> String
  params :: q -> M.Map String String
