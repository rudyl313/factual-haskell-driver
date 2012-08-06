module Main where

import System.Environment (getArgs)
import Network.Factual.API
import Data.Factual.Query.DiffsQuery
import Data.Factual.Response

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let token = generateToken oauthKey oauthSecret
  let query = DiffsQuery { table = Custom "canada-stable", start = 1339123455775, end = 1339124455775 }
  result <- executeQuery token query
  putStrLn $ "Status: " ++ status result
  putStrLn $ "Version: " ++ show (version result)
  putStrLn $ "Data: " ++ show (response result)
