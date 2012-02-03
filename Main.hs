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
  let query = TableQuery { table = Places
                         , queryMethod = Read
                         , limit = Just 50
                         , filters = Just "%7B%22name%22%3A%22Bar%20Hayama%22%7D" }
  putStrLn $ "http://api.v3.factual.com/" ++ toPath query
  payload <- runQuery creds query
  putStrLn $ show payload


data Credentials = Credentials String String


data Table = Places | USRestaurants | Global deriving Eq
instance Show Table where
  show Places = "places"
  show USRestaurants = "restaurants-us"
  show Global = "global"

data QueryMethod = Read | Schema deriving Eq
instance Show QueryMethod where
  show Read = "read"
  show Schema = "schema"



class Query q where
  toPath :: q -> String

data TableQuery = TableQuery { table :: Table
                             , queryMethod :: QueryMethod
                             , limit :: Maybe Int
                             , filters :: Maybe String
                             } deriving (Eq, Show)

instance Query TableQuery where
  toPath q = "t/"
           ++ (show $ table q)
           ++ "/"
           ++ (show $ queryMethod q)
           ++ "?"
           ++ (limitString $ limit q)
           ++ (filtersString $ filters q)

limitString :: Maybe Int -> String
limitString (Just x) = "limit=" ++ show x ++ "&"
limitString Nothing = ""

filtersString :: Maybe String -> String
filtersString (Just s) = "filters=" ++ s
filtersString Nothing = ""



runQuery :: (Query q) => Credentials -> q -> IO Value
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
