module Main where

import System.Environment (getArgs)
import Network.Factual.API
import Data.Factual.Response
import qualified Data.Map as M

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let token = generateToken oauthKey oauthSecret
  result <- get token "/t/places" $ M.fromList [("q", "starbucks")]
  putStrLn $ "Status: " ++ status result
  putStrLn $ "Version: " ++ show (version result)
  putStrLn $ "Data: " ++ show (response result)
