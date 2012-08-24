-- | This module exports the types used to create inserts.
module Data.Factual.Write.Insert
  (
    -- * Insert type
    Insert(..)
    -- * Required modules
  , module Data.Factual.Shared.Table
  ) where

import Data.Factual.Write
import Data.Factual.Shared.Table
import Data.Maybe (fromJust)
import Data.Factual.Utils
import qualified Data.Map as M

-- | The Insert type represents a Write to the API which performs an upsert
--   (a row can be updated or a new row can be written). The table and user
--   must be specified, while the factual ID is optional (omitted for new
--   rows). Finally the values are specified in a String to String Map.
data Insert = Insert { table     :: Table
                     , user      :: String
                     , factualId :: Maybe String
                     , values    :: M.Map String String
                     } deriving (Eq, Show)

-- The Insert type is a member of the Write typeclass so that it can be
-- sent as a post request to the API.
instance Write Insert where
  path   insert = pathString insert
  params _      = M.empty
  body   insert = M.fromList [ ("user", user insert) 
                             , ("values", valuesString (values insert)) ]

-- The following functions are helpers for the Write typeclass functions.
pathString :: Insert -> String
pathString insert
  | factualId insert == Nothing = (show $ table insert) ++ "/insert"
  | otherwise = (show $ table insert)
              ++ "/"
              ++ (fromJust $ factualId insert)
              ++ "/insert"

valuesString :: M.Map String String -> String
valuesString values = "{" ++ join "," (map valueString $ M.keys values) ++ "}"
  where valueString key = "\"" ++ key ++ "\":\"" ++
                          (fromJust $ M.lookup key values) ++ "\""
