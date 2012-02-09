-- | This module exports the type used to create schema queries.
module Data.Factual.SchemaQuery
  (
    -- * SchemaQuery type
    SchemaQuery(..)
  ) where

import Data.Factual.Query
import Data.Factual.Table

-- | A schema query is formed by simply supplying a Table to the value
--   constructor.
data SchemaQuery = SchemaQuery Table deriving (Eq, Show)

-- SchemaQuery is a member of Query typeclass so that it can generate a response.
instance Query SchemaQuery where
  toPath (SchemaQuery table) = (show $ table) ++ "schema"
