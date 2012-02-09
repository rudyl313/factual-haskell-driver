module Main where

import System (getArgs)
import Network.Factual.API
import Data.Factual.Credentials
import Data.Factual.Table
import Data.Factual.CrosswalkQuery
import Data.Factual.Response

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let token = generateToken (Credentials oauthKey oauthSecret)
  let query = CrosswalkQuery { factualId = Just "97598010-433f-4946-8fd5-4a6dd1639d77"
                             , limit = Nothing
                             , namespace = Nothing
                             , namespaceId = Nothing
                             , only = ["loopt"] }
  result <- makeRequest token query
  putStrLn $ "Status: " ++ status result
  putStrLn $ "Version: " ++ show (version result)
  putStrLn $ "Data: " ++ show (response result)
