module Data.Factual.Query where

class Query q where
  toPath :: q -> String
