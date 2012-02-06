module Data.Factual.CrosswalkQuery (CrosswalkQuery(..)) where

import Data.Factual.Query
import Data.Factual.Utils

data CrosswalkQuery = CrosswalkQuery { factualId :: Maybe String
                                     , limit :: Maybe Int
                                     , namespace :: Maybe String
                                     , namespaceId :: Maybe String
                                     , only :: [String]
                                     } deriving (Eq, Show)

instance Query CrosswalkQuery where
  toPath query = "/places/crosswalk?"
               ++ joinAndFilter [ factualIdString $ factualId query
                                , limitString $ limit query
                                , namespaceString $ namespace query
                                , namespaceIdString $ namespaceId query
                                , onlyString $ only query ]

factualIdString :: Maybe String -> String
factualIdString (Just id) = "factual_id=" ++ id
factualIdString Nothing   = ""

limitString :: Maybe Int -> String
limitString (Just lim) = "limit=" ++ (show lim)
limitString Nothing    = ""

namespaceString :: Maybe String -> String
namespaceString (Just namespace) = "namespace=" ++ namespace
namespaceString Nothing          = ""

namespaceIdString :: Maybe String -> String
namespaceIdString (Just id) = "namespace_id=" ++ id
namespaceIdString Nothing   = ""

onlyString :: [String] -> String
onlyString []   = ""
onlyString strs = "only=" ++ (join "," strs)
