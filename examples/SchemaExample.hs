module Main where

import System (getArgs)
import Network.Factual.API
import Data.Factual.Query.SchemaQuery
import Data.Factual.Response

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let token = generateToken (Credentials oauthKey oauthSecret)
  let query = SchemaQuery Places
  result <- makeRequest token query
  putStrLn $ "Status: " ++ status result
  putStrLn $ "Version: " ++ show (version result)
  putStrLn $ "Data: " ++ show (response result)
