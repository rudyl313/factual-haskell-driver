-- | This module exports the type which encapsulates Factual API responses. It
--   also provides some utility function that can be used to manipulate the
--   Aeson object which holds the data.
module Data.Factual.Response
  (
    -- * Response type
    Response(..)
    -- * Utility functions
  , fromValue
  , toList
  , lookupNumber
  , lookupString
  , lookupValue
  , lookupValueSafe
    -- * Aeson Value type
  , Value
  ) where

import Data.Maybe (fromJust)
import Data.Aeson
import Data.Attoparsec.Number
import qualified Data.HashMap.Lazy as L
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace

-- | A response object has a status (that will be ok if the query was successful
--   and error if the query failed), a version (which should always be 3.0) and
--   the actual response data which is an Aeson value.
data Response = Response { status       :: String
                         , version      :: Double
                         , response     :: Value
                         , errorMessage :: Maybe String
                         , errorType    :: Maybe String
                         } deriving (Eq, Show)

-- | This function is used by the API module to turn the Aeson value returned by
--   the API into a Response value.
fromValue :: Value -> Response
fromValue value
  | respStatus == "ok" = formValidResponse value
  | otherwise          = formErrorResponse value
  where respStatus = lookupString "status" value

-- | This function can be used to convert an Aeson Array value into a vanilla
--   list.
toList :: Value -> [Value]
toList (Array a) = V.toList a
toList v         = [v]

-- | This function can be used to extract a Double from an Aeson Object
--   (HashMap) value.
lookupNumber :: String -> Value -> Double
lookupNumber key hashmap = extractNumber $ lookupValue key hashmap

-- | This function can be used to extract a String from an Aeson Object
--   (HashMap) value.
lookupString :: String -> Value -> String
lookupString key hashmap = extractString $ lookupValue key hashmap

-- | This function can be used to extract any Aeson value from an Aeson Object
--   (HashMap) value.
lookupValue :: String -> Value -> Value
lookupValue key hashmap = if maybeValue == Nothing then Null else fromJust maybeValue
  where maybeValue = lookupValueSafe key hashmap


-- | This function can be used to safely extract any Aeson value from an Aeson
--    Object (HashMap) value.
lookupValueSafe :: String -> Value -> Maybe Value
lookupValueSafe key (Object x) = L.lookup (T.pack key) x

-- The following helper functions aid the lookup functions.
formErrorResponse :: Value -> Response
formErrorResponse value = Response { status = lookupString "status" value
                                   , version = lookupNumber "version" value
                                   , response = Null
                                   , errorMessage = Just $ lookupString "message" value
                                   , errorType = Just $ lookupString "error_type" value }

formValidResponse :: Value -> Response
formValidResponse value = Response { status = "ok"
                                   , version = lookupNumber "version" value
                                   , response = lookupValue "response" value
                                   , errorMessage = Nothing
                                   , errorType = Nothing }

extractString :: Value -> String
extractString (String x) = T.unpack x

extractNumber :: Value -> Double
extractNumber (Number x) = toDouble x

toDouble :: Number -> Double
toDouble (I x) = fromIntegral x
toDouble (D x) = x
