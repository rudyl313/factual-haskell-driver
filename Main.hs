module Main where

import System (getArgs)
import Network.Factual.API
import Data.Factual.TableQuery
import Data.Factual.Credentials

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let creds = Credentials oauthKey oauthSecret
  let query = TableQuery { table = Places
                         , queryMethod = Read
                         , searchTerms = []
                         , select = ["name"]
                         , limit = Just 50
                         , offset = Nothing
                         , includeCount = True
                         , geo = Just (Circle 34.06021 (-118.41828) 5000.0)
                         , filters = [Filter "name" "Stand"] }
  payload <- runQuery creds query
  putStrLn $ show payload
