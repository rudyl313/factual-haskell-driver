module Main where

import System.Environment (getArgs)
import Network.Factual.API
import Data.Factual.Query.MatchQuery
import Data.Factual.Response

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let options = Options { token = generateToken oauthKey oauthSecret, timeout = Nothing }
  let query = MatchQuery [ MatchStr "name" "McDonalds"
                         , MatchStr "address" "10451 Santa Monica Blvd" ]
  result <- executeQuery options query
  putStrLn $ "Status: " ++ status result
  putStrLn $ "Version: " ++ show (version result)
  putStrLn $ "Data: " ++ show (response result)
