-- | This module exports functions which are used to execute requests and handle
--   the OAuth authentication process.
module Network.Factual.API
  (
    -- * Authentication
    generateToken
    -- * Read functions
  , executeQuery
  , executeMultiQuery
  , get
    -- * Write functions
  , executeWrite
  , post
    -- * Debug functions
  , debugQuery
  , debugWrite
    -- * The hoauth Token type
  , Token(..)
  ) where

import Network.HTTP.Base (urlEncode)
import Data.Maybe (fromJust)
import Data.List (intersperse)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request (Request(..), Method(..), parseURL, fromList)
import Network.OAuth.Http.Response (Response(..))
import Network.OAuth.Http.CurlHttpClient (CurlClient(..))
import Data.Aeson (Value, decode)
import Data.Factual.Query
import Data.Factual.Utils
import qualified Data.Factual.Write as W
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Factual.Response as F

type Key    = String
type Secret = String
type Path   = String
type Params = M.Map String String
type Body   = M.Map String String

generateToken :: Key -> Secret -> Token
generateToken key secret = fromApplication $ Application key secret OOB

executeQuery :: (Query query) => Token -> query -> IO F.Response
executeQuery token query = get token (path query) (params query)

executeMultiQuery :: (Query query) => Token -> M.Map String query -> IO (M.Map String F.Response)
executeMultiQuery token multiMap = do
  let queryString = formMultiQueryString $ M.toList multiMap
  response <- get'' token queryString
  return $ formMultiResponse response $ M.keys multiMap

debugQuery :: (Query query) => query -> IO ()
debugQuery query = putStrLn $ "Query path: " ++ basePath ++ (formQueryString (path query) (params query))

executeWrite :: (W.Write write) => Token -> write -> IO F.Response
executeWrite token write = post token (W.path write) (W.params write) (W.body write)

debugWrite :: (W.Write write) => write -> IO ()
debugWrite write = do
  putStrLn ("Write path: " ++ basePath ++ W.path write)
  putStrLn "Write body:"
  putStrLn $ formParamsString $ W.body write

get :: Token -> Path -> Params -> IO F.Response
get token path params = get' token (formQueryString path params)

get' :: Token -> String -> IO F.Response
get' token queryString = do
  response <- get'' token queryString
  return $ F.fromValue $ extractJSON response

get'' :: Token -> String -> IO Response
get'' token queryString = do
  let fullPath = basePath ++ queryString
  let request = generateRequest fullPath
  execute token request

post :: Token -> Path -> Params -> Body -> IO F.Response
post token path params body = post' token queryString bodyString
  where queryString = formQueryString path params
        bodyString  = formParamsString body

post' :: Token -> String -> String -> IO F.Response
post' token queryString bodyString = do
  let fullPath = basePath ++ queryString
  let request = generatePostRequest fullPath bodyString
  response <- execute token request
  return $ F.fromValue $ extractJSON response

execute :: Token -> Request -> IO Response
execute token request = runOAuthM token $ setupOAuth request

formQueryString :: String -> M.Map String String -> String
formQueryString path params = path ++ "?" ++ (formParamsString params)

formParamsString :: M.Map String String -> String
formParamsString params = formParamsString' $ M.toList params

formParamsString' :: [(String, String)] -> String
formParamsString' paramList = join "&" $ map formParamParts filteredParams
  where filteredParams = filter (\(k,v) -> "" /= v) paramList

formParamParts :: (String, String) -> String
formParamParts (key, value) = key ++ "=" ++ (urlEncode value)

formMultiQueryString :: (Query query) => [(String, query)] -> String
formMultiQueryString ps = "/multi?queries=" ++ (urlEncode $ "{" ++ (join "," $ map queryPair ps) ++ "}")
  where queryPair (n,q) = "\"" ++ n ++ "\":\"" ++ (formQueryString (path q) (params q)) ++ "\""

formMultiResponse :: Response -> [String] -> M.Map String F.Response
formMultiResponse res ks = M.fromList $ map formPair ks
  where formPair k = (k, F.fromValue $ F.lookupValue k json)
        json       = extractJSON res

generateRequest :: String -> Request
generateRequest url = (fromJust $ parseURL url) { reqHeaders = (fromList headersList) }

generatePostRequest :: String -> String -> Request
generatePostRequest url body = baseRequest { reqHeaders = (fromList headersList)
                                           , method = POST
                                           , reqPayload = B.pack body }
  where baseRequest = (fromJust $ parseURL url)

setupOAuth :: Request -> OAuthMonadT IO Response
setupOAuth request = do
  oauthRequest <- signRq2 HMACSHA1 Nothing request
  serviceRequest CurlClient oauthRequest

extractJSON :: Response -> Value
extractJSON = fromJust . decode . rspPayload

headersList :: [(String, String)]
headersList = [("X-Factual-Lib", "factual-haskell-driver-0.4.0")]

basePath :: String
basePath = "http://api.v3.factual.com"
