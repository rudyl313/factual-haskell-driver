module Data.Factual.Search (Search(..)) where

import Data.Factual.Utils

data Search = AndSearch [String] | OrSearch [String] deriving Eq

instance Show Search where
  show (AndSearch terms) = join " " terms
  show (OrSearch terms) = join "," terms
