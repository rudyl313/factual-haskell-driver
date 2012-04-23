-- | This module exports the type used to create schema queries.
module Data.Factual.Query.SchemaQuery
  (
    -- * SchemaQuery type
    SchemaQuery(..)
    -- Required modules
  , module Data.Factual.Shared.Table
  ) where

import Data.Factual.Query
import Data.Factual.Shared.Table

-- | A schema query is formed by simply supplying a Table to the value
--   constructor.
data SchemaQuery = SchemaQuery Table deriving (Eq, Show)

-- SchemaQuery is a member of Query typeclass so that it can generate a response.
instance Query SchemaQuery where
  toPath (SchemaQuery table) = (show $ table) ++ "schema"
