-- | This module exports the Filter type used to create read and facet queries.
module Data.Factual.Shared.Filter
  (
    -- * Filter type
    Field
  , Filter(..)
    -- * Helper functions
  , filtersPair
  ) where

import Data.List.Utils (join, replace)

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
            | GreaterThan Field Double -- ^ A field must be greater than the given value.
            | GreaterThanOrEqualTo Field Double -- ^ A field must be greater than or equal to the given value.
            | LessThan Field Double -- ^ A field must be less than the given value.
            | LessThanOrEqualTo Field Double -- ^ A field must be less than or equal to the given value.
            | SearchFilter Field String -- ^ A field must match of full text search with the given string.
            | And [Filter] -- ^ Form an AND condition with the filters in the list.
            | Or [Filter] -- ^ Form an OR condition with the filters in the list.
            deriving Eq

-- Filter is a member of Show to help generate query strings.
instance Show Filter where
  show (EqualNum field num) = (showStr field) ++ ":" ++ (show num)
  show (EqualStr field str) = (showStr field) ++ ":" ++ (showStr str)
  show (NotEqualNum field num) = (showStr field) ++ ":{" ++ (show "$neq") ++ ":" ++ (show num) ++ "}"
  show (NotEqualStr field str) = (showStr field) ++ ":{" ++ (show "$neq") ++ ":" ++ (showStr str) ++ "}"
  show (InNumList field nums) = (showStr field) ++ ":{" ++ (show "$in") ++ ":[" ++ (join "," $ map show nums) ++ "]}"
  show (InStrList field strs) = (showStr field) ++ ":{" ++ (show "$in") ++ ":[" ++ (join "," $ map showStr strs) ++ "]}"
  show (NotInNumList field nums) = (showStr field) ++ ":{" ++ (show "$nin") ++ ":[" ++ (join "," $ map show nums) ++ "]}"
  show (NotInStrList field strs) = (showStr field) ++ ":{" ++ (show "$nin") ++ ":[" ++ (join "," $ map showStr strs) ++ "]}"
  show (BeginsWith field str) = (showStr field) ++ ":{" ++ (show "$bw") ++ ":" ++ (showStr str) ++ "}"
  show (NotBeginsWith field str) = (showStr field) ++ ":{" ++ (show "$nbw") ++ ":" ++ (showStr str) ++ "}"
  show (BeginsWithAny field strs) = (showStr field) ++ ":{" ++ (show "$bwin") ++ ":[" ++ (join "," $ map showStr strs) ++ "]}"
  show (NotBeginsWithAny field strs) = (showStr field) ++ ":{" ++ (show "$nbwin") ++ ":[" ++ (join "," $ map showStr strs) ++ "]}"
  show (IsBlank field) = (showStr field) ++ ":{\"$blank\":true}"
  show (IsNotBlank field) = (showStr field) ++ ":{\"$blank\":false}"
  show (GreaterThan field num) = (showStr field) ++ ":{" ++ (show "$gt") ++ ":" ++ (show num) ++ "}"
  show (GreaterThanOrEqualTo field num) = (showStr field) ++ ":{" ++ (show "$gte") ++ ":" ++ (show num) ++ "}"
  show (LessThan field num) = (showStr field) ++ ":{" ++ (show "$lt") ++ ":" ++ (show num) ++ "}"
  show (LessThanOrEqualTo field num) = (showStr field) ++ ":{" ++ (show "$lte") ++ ":" ++ (show num) ++ "}"
  show (SearchFilter field str) = (showStr field) ++ ":{" ++ (show "$search") ++ ":" ++ (showStr str) ++ "}"
  show (And filters) = (show "$and") ++ ":[" ++ (join "," $ map showFilter filters) ++ "]"
  show (Or filters) = (show "$or") ++ ":[" ++ (join "," $ map showFilter filters) ++ "]"

-- The following helper functions are used in generating query params.
showFilter :: Filter -> String
showFilter filter = "{" ++ (show filter) ++ "}"

showStr :: String -> String
showStr str = "\"" ++ replace "\"" "\\\"" str ++ "\""

filtersPair :: [Filter] -> (String, String)
filtersPair [] = ("filters", "")
filtersPair fs = ("filters", "{" ++ (join "," $ map show fs) ++ "}")
