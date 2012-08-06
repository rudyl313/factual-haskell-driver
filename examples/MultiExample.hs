module Main where

import System.Environment (getArgs)
import Network.Factual.API
import Data.Factual.Query.ReadQuery
import Data.Factual.Response
import qualified Data.Map as M

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let token = generateToken oauthKey oauthSecret
  let query1 = ReadQuery { table = Places
                         , search = AndSearch []
                         , select = ["name"]
                         , limit = Just 50
                         , offset = Nothing
                         , includeCount = True
                         , geo = Just (Circle 34.06021 (-118.41828) 5000.0)
                         , filters = [EqualStr "name" "Stand"] }
  let query2 = ReadQuery { table = Places
                         , search = AndSearch []
                         , select = ["name"]
                         , limit = Just 50
                         , offset = Nothing
                         , includeCount = True
                         , geo = Just (Circle 34.06021 (-118.41828) 5000.0)
                         , filters = [EqualStr "name" "Xerox"] }
  let multiQuery = M.fromList [("query1", query1), ("query2", query2)]
  multiResult <- executeMultiQuery token multiQuery
  let result1 = multiResult M.! "query1"
  let result2 = multiResult M.! "query2"
  putStrLn "query1:"
  putStrLn $ "Status: " ++ status result1
  putStrLn $ "Version: " ++ show (version result1)
  putStrLn $ "Data: " ++ show (response result1)
  putStrLn "query2:"
  putStrLn $ "Status: " ++ status result2
  putStrLn $ "Version: " ++ show (version result2)
  putStrLn $ "Data: " ++ show (response result2)
