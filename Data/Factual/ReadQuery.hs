module Data.Factual.ReadQuery
  ( ReadQuery(..)
  , Search(..)
  , Filter(..)
  , Circle(..)
  ) where

import Data.Factual.Query
import Data.Factual.Table
import Data.Factual.Utils

type Field = String
type Lat = Double
type Long = Double
type Radius = Double

data Search = AndSearch [String] | OrSearch [String] deriving Eq

instance Show Search where
  show (AndSearch terms) = join " " terms
  show (OrSearch terms) = join "," terms

data Filter = EqualNum Field Double
            | EqualStr Field String
            | NotEqualNum Field Double
            | NotEqualStr Field String
            | InNumList Field [Double]
            | InStrList Field [String]
            | NotInNumList Field [Double]
            | NotInStrList Field [String]
            | BeginsWith Field String
            | NotBeginsWith Field String
            | BeginsWithAny Field [String]
            | NotBeginsWithAny Field [String]
            | IsBlank Field
            | IsNotBlank Field
            | And [Filter]
            | Or [Filter]
            deriving Eq

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

data Circle = Circle Lat Long Radius deriving Eq

instance Show Circle where
  show (Circle lat long radius) = "{\"$circle\":{\"$center\":[" 
                                ++ (show lat) 
                                ++ ", "
                                ++ (show long)
                                ++ "],\"$meters\":"
                                ++ (show radius)
                                ++ "}}"

data ReadQuery = ReadQuery { table        :: Table
                           , search       :: Search
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
               ++ joinAndFilter [ searchString $ search query
                                , selectString $ select query
                                , limitString $ limit query
                                , offsetString $ offset query
                                , filtersString $ filters query
                                , geoString $ geo query
                                , includeCountString $ includeCount query ]

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

geoString :: Maybe Circle -> String
geoString (Just c) = "geo=" ++ show c
geoString Nothing = ""

includeCountString :: Bool -> String
includeCountString True = "include_count=true"
includeCountString False = "include_count=false"
