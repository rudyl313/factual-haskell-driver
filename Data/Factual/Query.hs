-- | This module exports the definition of the Query typeclass
module Data.Factual.Query
  (
  -- * Query typeclass
  Query(..)) where

-- | A member of the Query typeclass must define a toPath method which converts
--   the Query into a path String
class Query q where
  toPath :: q -> String
