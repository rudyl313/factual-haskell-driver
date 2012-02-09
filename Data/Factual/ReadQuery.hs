-- | This module exports the types used to create read queries.
module Data.Factual.ReadQuery
  (
    -- * ReadQuery type
    ReadQuery(..)
    -- * Search type
  , Search(..)
    -- * Filter type
  , Field
  , Filter(..)
    -- * Geo type
  , Lat
  , Long
  , Radius
  , Geo(..)
  ) where

import Data.Factual.Query
import Data.Factual.Table
import Data.Factual.Utils

-- | A Field is a String representation of the field name.
type Field = String
-- | A Lat is the latitude represented as a Double.
type Lat = Double
-- | A Long is the longitude represented as a Double.
type Long = Double
-- | A Radius is the radius of the circle as a Double in meters.
type Radius = Double

-- | This type is used to construct an ANDed or ORed search in a read query.
data Search = AndSearch [String] | OrSearch [String] deriving Eq

-- When a Search is shown it displays the string representation that will go
-- into the query string.
instance Show Search where
  show (AndSearch terms) = join " " terms
  show (OrSearch terms) = join "," terms

-- | The Filter type is used to represent various filters in a read query.
data Filter = EqualNum Field Double -- ^ A numeric field has to match a number exactly.
            | EqualStr Field String -- ^ A string field has to match a string exactly.
            | NotEqualNum Field Double -- ^ A numeric field must equal a specific number.
            | NotEqualStr Field String -- ^ A string field must equal a specific string.
            | InNumList Field [Double] -- ^ A numeric field must be equal to any of the numbers in a list.
            | InStrList Field [String] -- ^ A string field must be equal to any of the strings in a list.
            | NotInNumList Field [Double] -- ^ A numeric field must not be equal to any of the numbers in a list.
            | NotInStrList Field [String] -- ^ A string field must not be equal to any of the strings in a list.
            | BeginsWith Field String -- ^ A string field must begin with a specific string.
            | NotBeginsWith Field String -- ^ A string field must not begin with a specific string.
            | BeginsWithAny Field [String] -- ^ A string field must begin with any of the strings in a list.
            | NotBeginsWithAny Field [String] -- ^ A string field must not begin with any of the strings in a list.
            | IsBlank Field -- ^ A field must be blank.
            | IsNotBlank Field -- ^ A field must not be blank.
            | And [Filter] -- ^ Form an AND condition with the filters in the list.
            | Or [Filter] -- ^ Form an OR condition with the filters in the list.
            deriving Eq

-- Filter is a member of Show to help generate query strings.
instance Show Filter where
  show (EqualNum field num) = (show field) ++ ":" ++ (show num)
  show (EqualStr field str) = (show field) ++ ":" ++ (show str)
  show (NotEqualNum field num) = (show field) ++ ":{" ++ (show "$neq") ++ ":" ++ (show num) ++ "}"
  show (NotEqualStr field str) = (show field) ++ ":{" ++ (show "$neq") ++ ":" ++ (show str) ++ "}"
  show (InNumList field nums) = (show field) ++ ":{" ++ (show "$in") ++ ":[" ++ (join "," $ map show nums) ++ "]}"
  show (InStrList field strs) = (show field) ++ ":{" ++ (show "$in") ++ ":[" ++ (join "," $ map show strs) ++ "]}"
  show (NotInNumList field nums) = (show field) ++ ":{" ++ (show "$nin") ++ ":[" ++ (join "," $ map show nums) ++ "]}"
  show (NotInStrList field strs) = (show field) ++ ":{" ++ (show "$nin") ++ ":[" ++ (join "," $ map show strs) ++ "]}"
  show (BeginsWith field str) = (show field) ++ ":{" ++ (show "$bw") ++ ":" ++ (show str) ++ "}"
  show (NotBeginsWith field str) = (show field) ++ ":{" ++ (show "$nbw") ++ ":" ++ (show str) ++ "}"
  show (BeginsWithAny field strs) = (show field) ++ ":{" ++ (show "$bwin") ++ ":[" ++ (join "," $ map show strs) ++ "]}"
  show (NotBeginsWithAny field strs) = (show field) ++ ":{" ++ (show "$nbwin") ++ ":[" ++ (join "," $ map show strs) ++ "]}"
  show (IsBlank field) = (show field) ++ ":{\"$blank\":true}"
  show (IsNotBlank field) = (show field) ++ ":{\"$blank\":false}"
  show (And filters) = (show "$and") ++ ":[" ++ (join "," $ map showFilter filters) ++ "]"
  show (Or filters) = (show "$or") ++ ":[" ++ (join "," $ map showFilter filters) ++ "]"

-- | The Geo type is used to limit the search to specific geograph location.
--   Currently, only circles are supported. Supply a latitude, longitude and
--   radius in meters for the circle.
data Geo = Circle Lat Long Radius deriving Eq

-- Geo is a member of Show to help generate query strings.
instance Show Geo where
  show (Circle lat long radius) = "{\"$circle\":{\"$center\":[" 
                                ++ (show lat) 
                                ++ ", "
                                ++ (show long)
                                ++ "],\"$meters\":"
                                ++ (show radius)
                                ++ "}}"

-- | The ReadQuery type is used to construct read queries. A table should be
--   specified, but the rest of the query options are essentially optional
--   (you opt out using Nothing or an empty List for the value). The select is
--   a list of field names to include in the results. The limit and offset are
--   used to request a specific range of rows and includeCount will include the
--   count of returned rows if it is set to True.
data ReadQuery = ReadQuery { table        :: Table
                           , search       :: Search
                           , select       :: [String]
                           , limit        :: Maybe Int
                           , offset       :: Maybe Int
                           , filters      :: [Filter]
                           , geo          :: Maybe Geo
                           , includeCount :: Bool
                           } deriving (Eq, Show)

-- The ReadQuery type is a member of the Query typeclass so it can be used to
-- make a request.
instance Query ReadQuery where
  toPath query = (show $ table query)
               ++ "read?"
               ++ joinAndFilter [ searchString $ search query
                                , selectString $ select query
                                , limitString $ limit query
                                , offsetString $ offset query
                                , filtersString $ filters query
                                , geoString $ geo query
                                , includeCountString $ includeCount query ]

-- The following helper functions are used in generating query Strings.
showFilter :: Filter -> String
showFilter filter = "{" ++ (show filter) ++ "}"

searchString :: Search -> String
searchString (AndSearch []) = ""
searchString (OrSearch []) = ""
searchString search = "q=" ++ show search

selectString :: [String] -> String
selectString []      = ""
selectString selects = "select=" ++ (join "," selects)

limitString :: Maybe Int -> String
limitString (Just x) = "limit=" ++ show x
limitString Nothing = ""

offsetString :: Maybe Int -> String
offsetString (Just x) = "offset=" ++ show x
offsetString Nothing = ""

filtersString :: [Filter] -> String
filtersString [] = ""
filtersString fs = "filters={" ++ (join "," $ map show fs) ++ "}"

geoString :: Maybe Geo -> String
geoString (Just c) = "geo=" ++ show c
geoString Nothing = ""

includeCountString :: Bool -> String
includeCountString True = "include_count=true"
includeCountString False = "include_count=false"
