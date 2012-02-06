module Data.Factual.ResolveQuery
  ( ResolveQuery(..)
  , ResolveValue(..)
  ) where

import Data.Factual.Query
import Data.Factual.Utils

data ResolveValue = ResolveStr String String
                  | ResolveNum String Double
                  deriving Eq

instance Show ResolveValue where
  show (ResolveStr name str) = (show name) ++ ":" ++ (show str)
  show (ResolveNum name num) = (show name) ++ ":" ++ (show num)

data ResolveQuery = ResolveQuery [ResolveValue] deriving Eq

instance Query ResolveQuery where
  toPath (ResolveQuery values) = "/places/resolve?values={"
                               ++ (join "," $ map show values)
                               ++ "}"
