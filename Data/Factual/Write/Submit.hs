-- | This module exports the types used to create submits.
module Data.Factual.Write.Submit
  (
    -- * Submit type
    Submit(..)
    -- * Required modules
  , module Data.Factual.Shared.Table
  ) where

import Data.Factual.Write
import Data.Factual.Shared.Table
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.List.Utils (join)

-- | The Submit type represents a Write to the API which performs an upsert
--   (a row can be updated or a new row can be written). The table and user
--   must be specified, while the factual ID is optional (omitted for new
--   rows). Finally the values are specified in a String to String Map.
data Submit = Submit { table     :: Table
                     , user      :: String
                     , factualId :: Maybe String
                     , values    :: M.Map String String
                     } deriving (Eq, Show)

-- The Submit type is a member of the Write typeclass so that it can be
-- sent as a post request to the API.
instance Write Submit where
  path   submit = pathString submit
  params _      = M.empty
  body   submit = M.fromList [ ("user", user submit) 
                             , ("values", valuesString (values submit)) ]

-- The following functions are helpers for the Write typeclass functions.
pathString :: Submit -> String
pathString submit
  | factualId submit == Nothing = (show $ table submit) ++ "/submit"
  | otherwise = (show $ table submit)
              ++ "/"
              ++ (fromJust $ factualId submit)
              ++ "/submit"

valuesString :: M.Map String String -> String
valuesString values = "{" ++ join "," (map valueString $ M.keys values) ++ "}"
  where valueString key = "\"" ++ key ++ "\":\"" ++
                          (fromJust $ M.lookup key values) ++ "\""
