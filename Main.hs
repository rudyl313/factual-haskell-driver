module Main where

import System (getArgs)
import Network.Factual.API
import Data.Factual.ReadQuery
import Data.Factual.ResolveQuery
import Data.Factual.SchemaQuery
import Data.Factual.Credentials
import Data.Factual.Table
import Data.Factual.Search
import Data.Factual.Circle
import Data.Factual.Filter
import qualified Data.Factual.CrosswalkQuery as C

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let creds = Credentials oauthKey oauthSecret
  let query = ReadQuery { table = Places
                        , search = AndSearch []
                        , select = ["name"]
                        , limit = Just 50
                        , offset = Nothing
                        , includeCount = True
                        , geo = Just (Circle 34.06021 (-118.41828) 5000.0)
                        , filters = [EqualStr "name" "Stand"] }
  payload <- runQuery creds query
  putStrLn $ show payload
  let schema = SchemaQuery Places
  payload2 <- runQuery creds schema
  putStrLn $ show payload2
  let resolve = ResolveQuery [ResolveStr "name" "McDonalds"]
  payload3 <- runQuery creds resolve
  putStrLn $ show payload3
  let crosswalk = C.CrosswalkQuery { C.factualId = Just "97598010-433f-4946-8fd5-4a6dd1639d77"
                                   , C.limit = Nothing
                                   , C.namespace = Nothing
                                   , C.namespaceId = Nothing
                                   , C.only = ["loopt"] }
  payload4 <- runQuery creds crosswalk
  putStrLn $ show payload4
