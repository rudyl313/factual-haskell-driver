module Network.Factual.API (runQuery) where

import Data.Maybe (fromJust)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request (Request, parseURL)
import Network.OAuth.Http.Response (Response(..))
import Network.OAuth.Http.CurlHttpClient (CurlClient(..))
import Data.Aeson (Value, decode)
import Data.Factual.Query
import Data.Factual.Credentials
import qualified Data.Factual.Response as F

runQuery :: (Query query) => Credentials -> query -> IO F.Response
runQuery credentials query = do
  let token = generateToken credentials
  let fullpath = "http://api.v3.factual.com" ++ toPath query
  let request = generateRequest fullpath
  putStrLn fullpath -- temp
  response <- runOAuthM token $ setupOAuth request
  return $ F.fromValue $ extractJSON response

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
