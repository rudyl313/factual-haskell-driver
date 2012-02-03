module Data.Factual.TableQuery
  ( TableQuery(..)
  , Table(..)
  , QueryMethod(..)
  , Filter(..)
  , Circle(..)
  ) where

import Data.Factual.Query
import Data.List (intersperse)

data Table = Places | USRestaurants | Global deriving Eq

instance Show Table where
  show Places = "/t/places/"
  show USRestaurants = "/t/restaurants-us/"
  show Global = "/t/global/"

data QueryMethod = Read | Schema deriving Eq

instance Show QueryMethod where
  show Read = "read?"
  show Schema = "schema?"

data Circle = Circle Double Double Double deriving Eq

instance Show Circle where
  show (Circle lat long meters) = "{\"$circle\":{\"$center\":[" 
                                ++ (show lat) 
                                ++ ", "
                                ++ (show long)
                                ++ "],\"$meters\":"
                                ++ (show meters)
                                ++ "}}"

data Filter = Filter String String deriving Eq

instance Show Filter where
  show (Filter key value) = "\"" ++ key ++ "\":\"" ++ value ++ "\""

data TableQuery = TableQuery { table        :: Table
                             , queryMethod  :: QueryMethod
                             , searchTerms  :: [String]
                             , select       :: [String]
                             , limit        :: Maybe Int
                             , offset       :: Maybe Int
                             , filters      :: [Filter]
                             , geo          :: Maybe Circle
                             , includeCount :: Bool
                             } deriving (Eq, Show)

instance Query TableQuery where
  toPath query =  (show $ table query)
               ++ (show $ queryMethod query)
               ++ (searchTermsString $ searchTerms query)
               ++ (selectString $ select query)
               ++ (limitString $ limit query)
               ++ (offsetString $ offset query)
               ++ (filtersString $ filters query)
               ++ (geoString $ geo query)
               ++ (includeCountString $ includeCount query)

searchTermsString :: [String] -> String
searchTermsString terms = "q=" ++ (join "," terms) ++ "&"

selectString :: [String] -> String
selectString selects = "select=" ++ (join "," selects) ++ "&"

limitString :: Maybe Int -> String
limitString (Just x) = "limit=" ++ show x ++ "&"
limitString Nothing = ""

offsetString :: Maybe Int -> String
offsetString (Just x) = "offset=" ++ show x ++ "&"
offsetString Nothing = ""

filtersString :: [Filter] -> String
filtersString fs = "filters={" ++ (join "," $ map show fs) ++ "}&"

geoString :: Maybe Circle -> String
geoString (Just c) = "geo=" ++ (show c) ++ "&"
geoString Nothing = ""

includeCountString :: Bool -> String
includeCountString True = "include_count=true"
includeCountString False = "include_count=false"

join :: [a] -> [[a]] -> [a]
join delim xs = concat (intersperse delim xs)
