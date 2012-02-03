module Network.Factual.API where

import Data.Factual.Query
import Data.Factual.Credentials
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.CurlHttpClient
import Data.Maybe
import Data.Aeson

runQuery :: (Query query) => Credentials -> query -> IO Value
runQuery credentials query = do
  let token = generateToken credentials
  let request = generateRequest ("http://api.v3.factual.com/" ++ toPath query)
  response <- runOAuthM token $ setupOAuth request
  return $ extractJSON response

generateToken :: Credentials -> Token
generateToken (Credentials key secret) = fromApplication $ Application key secret OOB

generateRequest :: String -> Request
generateRequest = fromJust . parseURL

setupOAuth :: Request -> OAuthMonadT IO Response
setupOAuth request = do
  oauthRequest <- signRq2 HMACSHA1 Nothing request
  serviceRequest CurlClient oauthRequest

extractJSON :: Response -> Value
extractJSON = fromJust . decode . rspPayload
