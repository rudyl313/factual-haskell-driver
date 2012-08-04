-- | This module exports the type used to create schema queries.
module Data.Factual.Query.SchemaQuery
  (
    -- * SchemaQuery type
    SchemaQuery(..)
    -- * Required modules
  , module Data.Factual.Shared.Table
  ) where

import Data.Factual.Query
import Data.Factual.Shared.Table
import qualified Data.Map as M

-- | A schema query is formed by simply supplying a Table to the value
--   constructor.
data SchemaQuery = SchemaQuery Table deriving (Eq, Show)

-- SchemaQuery is a member of Query typeclass so that it can generate a response.
instance Query SchemaQuery where
  path   (SchemaQuery table) = (show $ table) ++ "/schema"
  params _                   = M.empty
