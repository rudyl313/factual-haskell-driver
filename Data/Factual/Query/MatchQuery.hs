-- | This module exports the type used to create match queries.
module Data.Factual.Query.MatchQuery
  (
    -- * MatchQuery type
    MatchQuery(..)
    -- * MatchValue type
  , MatchValue(..)
  ) where

import Data.Factual.Query
import qualified Data.Map as M
import Data.List.Utils (join)

-- | A match value can either be a String or a Number (Double). The first
--   argument is the name of the field and the second argument is the input
--   value.
data MatchValue = MatchStr String String
                | MatchNum String Double
                deriving Eq

-- MatchValue is a member of the Show typeclass to help generate query Strings.
instance Show MatchValue where
  show (MatchStr name str) = (show name) ++ ":" ++ (show str)
  show (MatchNum name num) = (show name) ++ ":" ++ (show num)

-- | A match query is formed as an array of match values. These values will
--   be compared with Factual records to return a cleaner, more canonical row
--   of data.
data MatchQuery = MatchQuery [MatchValue] deriving Eq

-- MatchQuery is a member of the Query typeclass so that it can be used to
-- make requests.
instance Query MatchQuery where
  path   _                   = "/places/match"
  params (MatchQuery values) = M.fromList [("values", valuesString)]
    where valuesString = "{" ++ (join "," $ map show values) ++ "}"
