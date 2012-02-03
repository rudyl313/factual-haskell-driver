module Main where

import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.CurlHttpClient
import Data.Maybe
import Data.Aeson
import System (getArgs)

main :: IO()
main = do
  args <- getArgs
  let oauthKey = head args
  let oauthSecret = last args
  let creds = Credentials oauthKey oauthSecret
  let url = "http://api.v3.factual.com/t/places/read?limit=50&filters=%7B%22name%22%3A%22Bar%20Hayama%22%7D"
  payload <- runQuery creds url
  putStrLn $ show payload



data Credentials = Credentials String String


runQuery :: Credentials -> String -> IO Value
runQuery credentials url = do
  let token = generateToken credentials
  let request = generateRequest url
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
