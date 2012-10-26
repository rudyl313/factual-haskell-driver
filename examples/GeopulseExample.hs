module Main where

import System.Environment (getArgs)
import Network.Factual.API
import Data.Factual.Query.GeopulseQuery
import Data.Factual.Response

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let options = Options { token = generateToken oauthKey oauthSecret, timeout = Nothing }
  let query = GeopulseQuery { geo    = Point 34.06021 (-118.41828)
                            , select = ["commercial_density"] }
  result <- executeQuery options query
  putStrLn $ "Status: " ++ status result
  putStrLn $ "Version: " ++ show (version result)
  putStrLn $ "Data: " ++ show (response result)
