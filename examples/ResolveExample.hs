module Main where

import System (getArgs)
import Network.Factual.API
import Data.Factual.Credentials
import Data.Factual.Shared.Table
import Data.Factual.Query.ResolveQuery
import Data.Factual.Response

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let token = generateToken (Credentials oauthKey oauthSecret)
  let query = ResolveQuery [ ResolveStr "name" "McDonalds"
                           , ResolveStr "address" "10451 Santa Monica Blvd" ]
  result <- makeRequest token query
  putStrLn $ "Status: " ++ status result
  putStrLn $ "Version: " ++ show (version result)
  putStrLn $ "Data: " ++ show (response result)
