module Data.Factual.Filter (Filter(..)) where

import Data.Factual.Utils

type Field = String

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
