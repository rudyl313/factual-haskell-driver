module Data.Factual.TableQuery where

import Data.Factual.Query

data Table = Places | USRestaurants | Global deriving Eq
instance Show Table where
  show Places = "places"
  show USRestaurants = "restaurants-us"
  show Global = "global"

data QueryMethod = Read | Schema deriving Eq
instance Show QueryMethod where
  show Read = "read"
  show Schema = "schema"




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
