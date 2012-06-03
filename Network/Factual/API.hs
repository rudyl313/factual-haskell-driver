-- | This module exports functions which are used to execute queries and handle
--   the OAuth authentication process.
module Network.Factual.API
  (
    -- * Authentication
    generateToken
    -- * Read functions
  , makeRequest
  , makeRawRequest
    -- * Write functions
  , sendWrite
    -- * The hoauth Token type
  , Token(..)
  ) where

import Data.Maybe (fromJust)
import Data.List (intersperse)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request (Request(..), Method(..), parseURL, fromList)
import Network.OAuth.Http.Response (Response(..))
import Network.OAuth.Http.CurlHttpClient (CurlClient(..))
import Data.Aeson (Value, decode)
import Data.Factual.Query
import Data.Factual.Write
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
makeRequest token query = makeRawRequest token (toPath query)

-- | This function can be used to make raw read requests for any path. You pass
--   in your Token and the path of your request (e.g. \"\/t\/places?q=starbucks\")
makeRawRequest :: Token -> String -> IO F.Response
makeRawRequest token queryString = do
  let fullpath = basePath ++ queryString
  let request = generateRequest fullpath
  makeRequest' token request

-- | This function can be used to make multi queries. You pass in a list of names
--   and a list of queries (of equal length) and a single queries is made to the
--   API.
makeMultRequest :: (Query query) => Token -> [String] -> [query] -> IO F.Response
makeMultRequest token names queries
  | length names /= length queries = error "The number of names must equal the number of queries"
  | otherwise                      = makeRawRequest token $ multiQueryString names queries

-- | This function takes an OAuth token and a Write and retunrs and IO action
--   which sends the Write to API and returns a Response.
sendWrite :: (Write write) => Token -> write -> IO F.Response
sendWrite token write = do
  let fullpath = basePath ++ path write
  let request = generatePostRequest fullpath (body write)
  makeRequest' token request

-- The following helper functions aid the exported API functions
makeRequest' :: Token -> Request -> IO F.Response
makeRequest' token request = do
  response <- runOAuthM token $ setupOAuth request
  return $ F.fromValue $ extractJSON response

multiQueryString :: (Query query) => [String] -> [query] -> String
multiQueryString ns qs = "/multi?queries={" ++ (join "," $ zipWith queryPair ns qs) ++ "}"
  where queryPair n q = "\"" ++ n ++ "\":\"" ++ toPath q ++ "\""

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

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
headersList = [("X-Factual-Lib", "factual-haskell-driver-0.2.0")]

basePath :: String
basePath = "http://api.v3.factual.com"
