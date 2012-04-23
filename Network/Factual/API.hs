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
import Network.OAuth.Consumer
import Network.OAuth.Http.Request (Request(..), parseURL, fromList)
import Network.OAuth.Http.Response (Response(..))
import Network.OAuth.Http.CurlHttpClient (CurlClient(..))
import Data.Aeson (Value, decode)
import Data.Factual.Query
import Data.Factual.Write
import Data.Factual.Credentials
import qualified Data.Factual.Response as F

-- | This function takes a set of credentials and returns an OAuth token that
--   can be used to make requests.
generateToken :: Credentials -> Token
generateToken (Credentials key secret) = fromApplication $ Application key secret OOB

-- | This function takes an OAuth token and a query (which is member of the
--   Query typeclass) and returns an IO action which will fetch a response from
--   the Factual API.
makeRequest :: (Query query) => Token -> query -> IO F.Response
makeRequest token query = makeRawRequest token (toPath query)

-- | This function can be used to make raw read requests for any path. You pass
--   in your Token and the path of your request (e.g. \"\/t\/places?q=starbucks\")
makeRawRequest :: Token -> String -> IO F.Response
makeRawRequest token queryString = do
  let fullpath = "http://api.v3.factual.com" ++ queryString
  let request = generateRequest fullpath
  response <- runOAuthM token $ setupOAuth request
  return $ F.fromValue $ extractJSON response

-- | This function takes an OAuth token and a Write and retunrs and IO action
--   which sends the Write to API and returns a Response.
sendWrite :: (Write write) => Token -> write -> IO F.Response
sendWrite token write = undefined

-- The following helper functions aid the exported API functions
generateRequest :: String -> Request
generateRequest url = (fromJust $ parseURL url) { reqHeaders = (fromList headersList) }

setupOAuth :: Request -> OAuthMonadT IO Response
setupOAuth request = do
  oauthRequest <- signRq2 HMACSHA1 Nothing request
  serviceRequest CurlClient oauthRequest

extractJSON :: Response -> Value
extractJSON = fromJust . decode . rspPayload

headersList :: [(String, String)]
headersList = [("X-Factual-Lib", "factual-haskell-driver-0.1.2")]
