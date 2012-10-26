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
  let options = Options { token = generateToken oauthKey oauthSecret, timeout = Nothing }
  result <- get options "/t/places" $ M.fromList [("q", "starbucks")]
  putStrLn $ "Status: " ++ status result
  putStrLn $ "Version: " ++ show (version result)
  putStrLn $ "Data: " ++ show (response result)
