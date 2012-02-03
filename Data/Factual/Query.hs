module Data.Factual.Query (Query(..)) where

class Query q where
  toPath :: q -> String
