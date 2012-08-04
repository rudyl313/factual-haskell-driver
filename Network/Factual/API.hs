-- | This module exports functions which are used to execute queries and handle
--   the OAuth authentication process.
module Network.Factual.API
  (
    -- * Authentication
    generateToken
    -- * Read functions
  , makeRequest
  , makeRawRequest
  , formRelativePath
  , makeRawRequest'
  , makeMultiRequest
    -- * Write functions
  , sendWrite
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

-- | The Key is the oauth key as a String.
type Key = String
-- | The Secret is the oauth secret as a String.
type Secret = String

-- | This function takes a set of credentials and returns an OAuth token that
--   can be used to make requests.
generateToken :: Key -> Secret -> Token
generateToken key secret = fromApplication $ Application key secret OOB

-- | This function takes an OAuth token and a query (which is member of the
--   Query typeclass) and returns an IO action which will fetch a response from
--   the Factual API.
makeRequest :: (Query query) => Token -> query -> IO F.Response
makeRequest token query = makeRawRequest token (path query) (params query)

makeRawRequest :: Token -> String -> M.Map String String -> IO F.Response
makeRawRequest token path params = makeRawRequest' token relativePath
  where relativePath = formRelativePath' path params

-- | This function can be used to make raw read requests for any path. You pass
--   in your Token and the path of your request (e.g. \"\/t\/places?q=starbucks\")
makeRawRequest' :: Token -> String -> IO F.Response
makeRawRequest' token queryString = do
  response <- makeRawRequest'' token queryString
  return $ F.fromValue $ extractJSON response

-- | This function can be used to make multi queries. You pass in a Map of Strings
--   to queries and a single query is made to the API. The result is a Map of the
--   same keys to regular response values.
makeMultiRequest :: (Query query) => Token -> M.Map String query -> IO (M.Map String F.Response)
makeMultiRequest token mult = do
  let queryString = multiQueryString $ M.toList mult
  response <- makeRawRequest'' token queryString
  return $ formMultiResponse response $ M.keys mult

-- | This function takes an OAuth token and a Write and retunrs and IO action
--   which sends the Write to API and returns a Response.
sendWrite :: (W.Write write) => Token -> write -> IO Response
sendWrite token write = do
  let fullpath = basePath ++ W.path write
  let request = generatePostRequest fullpath (W.body write)
  makeRequest' token request

-- | This function takes a query and prints out the path for debugging purposes
debugQuery :: (Query query) => query -> IO ()
debugQuery query = putStrLn $ "Query path: " ++ basePath ++ (formRelativePath' (path query) (params query))

-- | This function takes a write and prints out the path and body for debugging
--   purposes.
debugWrite :: (W.Write write) => write -> IO ()
debugWrite write = do
  putStrLn ("Write path: " ++ basePath ++ W.path write)
  putStrLn "Write body:"
  putStrLn $ W.body write

-- The following helper functions aid the exported API functions
makeRawRequest'' :: Token -> String -> IO Response
makeRawRequest'' token queryString = do
  let fullpath = basePath ++ queryString
  let request = generateRequest fullpath
  makeRequest' token request

makeRequest' :: Token -> Request -> IO Response
makeRequest' token request = runOAuthM token $ setupOAuth request

formRelativePath :: (Query q) => q -> String
formRelativePath query = formRelativePath' (path query) (params query)

formRelativePath' :: String -> M.Map String String -> String
formRelativePath' path params = path ++ "?" ++ (formParamsString params)

formParamsString :: M.Map String String -> String
formParamsString params = formParamsString' $ M.toList params

formParamsString' :: [(String, String)] -> String
formParamsString' paramList = join "&" $ paramParts
  where paramParts     = map formParamParts filteredParams
        filteredParams = filter nonEmpty paramList
        nonEmpty (k,v) = "" /= v

formParamParts :: (String, String) -> String
formParamParts (key, value) = key ++ "=" ++ (urlEncode value)

multiQueryString :: (Query query) => [(String, query)] -> String
multiQueryString ps = "/multi?queries=" ++ (urlEncode $ "{" ++ (join "," $ map queryPair ps) ++ "}")
  where queryPair (n,q) = "\"" ++ n ++ "\":\"" ++ (formRelativePath' (path q) (params q)) ++ "\""

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
