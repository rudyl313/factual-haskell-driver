-- | This module exports the types used to create contributions.
module Data.Factual.Write.Contribute
  (
    -- * Contribute type
    Contribute(..)
    -- * Required modules
  , module Data.Factual.Shared.Table
  ) where

import Data.Factual.Write
import Data.Factual.Shared.Table
import Data.Maybe (fromJust)
import Data.Factual.Utils
import qualified Data.Map as M

-- | The Contribute type represents a Write to the API which performs an upsert
--   (a row can be updated or a new row can be written). The table and user
--   must be specified, while the factual ID is optional (omitted for new
--   rows). Finally the values are specified in a String to String Map.
data Contribute = Contribute { table     :: Table
                             , user      :: String
                             , factualId :: Maybe String
                             , values    :: M.Map String String
                             } deriving (Eq, Show)

-- The Contribute type is a member of the Write typeclass so that it can be
-- sent as a post request to the API.
instance Write Contribute where
  path contribute = pathString contribute
  body contribute = "user=" ++ (user contribute) ++ "&" ++
                    "values=" ++ valuesString (values contribute)

-- The following functions are helpers for the Write typeclass functions.
pathString :: Contribute -> String
pathString contribute
  | factualId contribute == Nothing = (show $ table contribute) ++ "contribute"
  | otherwise = (show $ table contribute) ++
                (fromJust $ factualId contribute) ++
                "/contribute"

valuesString :: M.Map String String -> String
valuesString values = "{" ++ join "," (map valueString $ M.keys values) ++ "}"
  where valueString key = "\"" ++ key ++ "\":\"" ++
                          (fromJust $ M.lookup key values) ++ "\""
