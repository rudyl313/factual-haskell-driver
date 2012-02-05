module Main where

import System (getArgs)
import Network.Factual.API
import Data.Factual.ReadQuery
import Data.Factual.Credentials
import Data.Factual.Table
import Data.Factual.Circle
import Data.Factual.Filter

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let creds = Credentials oauthKey oauthSecret
  let query = ReadQuery { table = Places
                        , searchTerms = []
                        , select = ["name"]
                        , limit = Just 50
                        , offset = Nothing
                        , includeCount = True
                        , geo = Just (Circle 34.06021 (-118.41828) 5000.0)
                        , filters = [EqualStr "name" "Stand"] }
  payload <- runQuery creds query
  putStrLn $ show payload
