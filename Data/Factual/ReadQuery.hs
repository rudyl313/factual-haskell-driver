module Data.Factual.ReadQuery (ReadQuery(..)) where

import Data.Factual.Query
import Data.Factual.Table
import Data.Factual.Circle
import Data.Factual.Filter
import Data.Factual.Utils

data ReadQuery = ReadQuery { table        :: Table
                           , searchTerms  :: [String]
                           , select       :: [String]
                           , limit        :: Maybe Int
                           , offset       :: Maybe Int
                           , filters      :: [Filter]
                           , geo          :: Maybe Circle
                           , includeCount :: Bool
                           } deriving (Eq, Show)

instance Query ReadQuery where
  toPath query = (show $ table query)
               ++ "read?"
               ++ (searchTermsString $ searchTerms query)
               ++ (selectString $ select query)
               ++ (limitString $ limit query)
               ++ (offsetString $ offset query)
               ++ (filtersString $ filters query)
               ++ (geoString $ geo query)
               ++ (includeCountString $ includeCount query)

searchTermsString :: [String] -> String
searchTermsString [] = ""
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