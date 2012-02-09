-- | This module exports functions which are used to execute queries and handle
--   the OAuth authentication process.
module Network.Factual.API
  (
    -- * API functions
    makeRequest
  , generateToken
    -- * The hoauth Token type
  , Token(..)
  ) where

import Data.Maybe (fromJust)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request (Request, parseURL)
import Network.OAuth.Http.Response (Response(..))
import Network.OAuth.Http.CurlHttpClient (CurlClient(..))
import Data.Aeson (Value, decode)
import Data.Factual.Query
import Data.Factual.Credentials
import qualified Data.Factual.Response as F

-- | This function takes an OAuth token and a query (which is member of the
--   Query typeclass and returns an IO action which will fetch a response from
--   the Factual API.
makeRequest :: (Query query) => Token -> query -> IO F.Response
makeRequest token query = do
  let fullpath = "http://api.v3.factual.com" ++ toPath query
  let request = generateRequest fullpath
  response <- runOAuthM token $ setupOAuth request
  return $ F.fromValue $ extractJSON response

-- | This function takes a set of credentials and returns an OAuth token that
--   can be used to make requests.
generateToken :: Credentials -> Token
generateToken (Credentials key secret) = fromApplication $ Application key secret OOB

-- The following helper functions aid the exported API functions
generateRequest :: String -> Request
generateRequest = fromJust . parseURL

setupOAuth :: Request -> OAuthMonadT IO Response
setupOAuth request = do
  oauthRequest <- signRq2 HMACSHA1 Nothing request
  serviceRequest CurlClient oauthRequest

extractJSON :: Response -> Value
extractJSON = fromJust . decode . rspPayload
