-- | This module exports the type used to create crosswalk queries.
module Data.Factual.Query.CrosswalkQuery
  (
   -- * CrosswalkQuery type
   CrosswalkQuery(..)
  ) where

import Data.Factual.Query
import Data.Factual.Utils

-- | A crosswalk query can be formed by specifying a factual id and
--   (optionally) a list of namespaces to only include when attempting to find
--   the ids for various namespaces, or by specifying a namespace and namespace
--   id in order to find the factual id. An optional limit can be set as well.
data CrosswalkQuery = CrosswalkQuery { factualId :: Maybe String
                                     , limit :: Maybe Int
                                     , namespace :: Maybe String
                                     , namespaceId :: Maybe String
                                     , only :: [String]
                                     } deriving (Eq, Show)

-- A crosswalk query is a member of the Query typeclass so it can generate a
-- path to be queried.
instance Query CrosswalkQuery where
  toPath query = "/places/crosswalk?"
               ++ joinAndFilter [ factualIdString $ factualId query
                                , limitString $ limit query
                                , namespaceString $ namespace query
                                , namespaceIdString $ namespaceId query
                                , onlyString $ only query ]

-- The following helper functions are used to generate the separate parts of
-- the query path.
factualIdString :: Maybe String -> String
factualIdString (Just id) = "factual_id=" ++ id
factualIdString Nothing   = ""

namespaceString :: Maybe String -> String
namespaceString (Just namespace) = "namespace=" ++ namespace
namespaceString Nothing          = ""

namespaceIdString :: Maybe String -> String
namespaceIdString (Just id) = "namespace_id=" ++ id
namespaceIdString Nothing   = ""

onlyString :: [String] -> String
onlyString []   = ""
onlyString strs = "only=" ++ (join "," strs)
