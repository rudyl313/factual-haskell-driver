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
  ) where

import Data.Maybe (fromJust)
import Data.Aeson
import Data.Attoparsec.Number
import qualified Data.HashMap.Lazy as L
import qualified Data.Text as T
import qualified Data.Vector as V

-- | A response object has a status (that will be ok if the query was successful
--   and error if the query failed), a version (which should always be 3.0) and
--   the actual response data which is an Aeson value.
data Response = Response { status   :: String
                         , version  :: Double
                         , response :: Value
                         } deriving (Eq, Show)

-- | This function is used by the API module to turn the Aeson value returned by
--   the API into a Response value.
fromValue :: Value -> Response
fromValue value = Response { status = lookupString "status" value
                           , version = lookupNumber "version" value
                           , response = lookupValue "response" value }

toList :: Value -> [Value]
toList (Array a) = V.toList a
toList v         = [v]

lookupNumber :: String -> Value -> Double
lookupNumber key hashmap = extractNumber $ lookupValue key hashmap

lookupString :: String -> Value -> String
lookupString key hashmap = extractString $ lookupValue key hashmap

lookupValue :: String -> Value -> Value
lookupValue key hashmap = fromJust $ lookupValueSafe key hashmap

lookupValueSafe :: String -> Value -> Maybe Value
lookupValueSafe key (Object x) = L.lookup (T.pack key) x

extractString :: Value -> String
extractString (String x) = T.unpack x

extractNumber :: Value -> Double
extractNumber (Number x) = toDouble x

toDouble :: Number -> Double
toDouble (I x) = fromIntegral x
toDouble (D x) = x
