-- | This module exports the Filter type used to create read and facet queries.
module Data.Factual.Shared.Filter
  (
    -- * Filter type
    Field
  , Filter(..)
    -- * Helper functions
  , filtersString
  ) where

import Data.Factual.Utils

-- | A Field is a String representation of the field name.
type Field = String

-- | The Filter type is used to represent various filters in a read or facets query.
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

-- The following helper functions are used in generating query Strings.
showFilter :: Filter -> String
showFilter filter = "{" ++ (show filter) ++ "}"

filtersString :: [Filter] -> String
filtersString [] = ""
filtersString fs = "filters={" ++ (join "," $ map show fs) ++ "}"
