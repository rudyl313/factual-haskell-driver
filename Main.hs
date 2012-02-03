module Main where

import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.CurlHttpClient
import Data.Maybe
-- import qualified Data.Text.Lazy as T
-- #import qualified Data.Text.Lazy.Encoding as TE
import Data.Aeson
import System (getArgs)

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let creds = Credentials oauthKey oauthSecret
  let url = "http://api.v3.factual.com/t/places/read?limit=50&filters=%7B%22name%22%3A%22Bar%20Hayama%22%7D"
  payload <- getResponse creds url
  putStrLn $ show payload



data Credentials = Credentials String String


getResponse :: Credentials -> String -> IO (Maybe Value)
getResponse credentials url = do
  let token = generateToken credentials
  let srvUrl = fromJust . parseURL $ url
  response <- getResponse' token srvUrl
  return $ extractJSON response

getResponse' :: Token -> Request -> IO Response
getResponse' token request = runOAuthM token $ setupOAuth request

extractJSON :: Response -> Maybe Value
extractJSON response = decode $ rspPayload response
-- extractJSON :: Response -> String
-- extractJSON response = T.unpack . TE.decodeUtf8 $ rspPayload response

setupOAuth :: Request -> OAuthMonadT IO Response
setupOAuth request = do
  oauthRequest <- signRq2 HMACSHA1 Nothing request
  serviceRequest CurlClient oauthRequest

generateToken :: Credentials -> Token
generateToken (Credentials key secret) = fromApplication $ Application key secret OOB
