module Main where

import System (getArgs)
import Network.Factual.API
import Data.Factual.Credentials
import Data.Factual.Response

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let token = generateToken (Credentials oauthKey oauthSecret)
  result <- makeRawRequest token "/t/places?q=starbucks"
  putStrLn $ "Status: " ++ status result
  putStrLn $ "Version: " ++ show (version result)
  putStrLn $ "Data: " ++ show (response result)
