module Data.Factual.Response
  ( Response(..)
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

data Response = Response { status   :: String
                         , version  :: Double
                         , response :: Value
                         } deriving (Eq, Show)

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
