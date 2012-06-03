module Main where

import System.Environment (getArgs)
import Network.Factual.API
import Data.Factual.Response

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let token = generateToken oauthKey oauthSecret
  result <- makeRawRequest token "/t/places?q=starbucks"
  putStrLn $ "Status: " ++ status result
  putStrLn $ "Version: " ++ show (version result)
  putStrLn $ "Data: " ++ show (response result)
