-- | This module exports the Search type used to create read and facet queries.
module Data.Factual.Shared.Search
  (
    -- * Search type
    Search(..)
    -- * Helper functions
  , searchPair
  ) where

import Data.Factual.Utils
import Network.HTTP.Base (urlEncode)

-- | This type is used to construct an ANDed or ORed search in a query.
data Search = AndSearch [String] | OrSearch [String] | NoSearch deriving Eq

-- When a Search is shown it displays the string representation that will go
-- into the query string.
instance Show Search where
  show (AndSearch terms) = join " " terms
  show (OrSearch terms)  = join "," terms
  show NoSearch          = ""

-- Helper functions
searchPair :: Search -> (String, String)
searchPair search = ("q", show search)
