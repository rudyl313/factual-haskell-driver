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
                         , limit = Just 50
                         , filters = Just "%7B%22name%22%3A%22Bar%20Hayama%22%7D" }
  payload <- runQuery creds query
  putStrLn $ show payload
