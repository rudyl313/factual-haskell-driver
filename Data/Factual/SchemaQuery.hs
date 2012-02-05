module Data.Factual.SchemaQuery (SchemaQuery(..)) where

import Data.Factual.Query
import Data.Factual.Table

data SchemaQuery = SchemaQuery Table deriving (Eq, Show)

instance Query SchemaQuery where
  toPath (SchemaQuery table) = (show $ table) ++ "schema"
