-- | This module exports the types used to create flag writes.
module Data.Factual.Write.Flag
  (
    -- * Flag type
    Flag(..)
    -- * Problem type
  , Problem(..)
  ) where

import Data.Factual.Write
import Data.Factual.Shared.Table
import Data.Maybe (fromJust)

-- | A Problem represents what is wrong with the row being flagged
data Problem = Duplicate
             | Nonexistent
             | Inaccurate
             | Inappropriate
             | Spam
             | Other
             deriving (Eq, Show)

-- | The Flag type represents a Write to be made to the API which flags a
--   row as having some kind of problem. The table and factualId identify the
--   problematic row, while the problem indicates the type of issue the row
--   has. The user is specified as a string. Other fields such as comment and
--   reference are optional. The debug flag is used to write in debug mode.
data Flag = Flag { table     :: Table
                 , factualId :: String
                 , problem   :: Problem
                 , user      :: String
                 , comment   :: Maybe String
                 , debug     :: Bool
                 , reference :: Maybe String
                 } deriving (Eq, Show)

-- The Flag type is a member of the Write typeclass so it can be sent as a post
-- request to the API.
instance Write Flag where
  path flag = (show $ table flag) ++ (factualId flag) ++ "/flag"
  body flag = "problem=" ++ (show $ problem flag) ++ "&" ++
              "user=" ++ (user flag) ++ "&" ++
              (commentString flag) ++
              (debugString flag) ++
              (referenceString flag)

-- The following functions are helpers for the body function
commentString :: Flag -> String
commentString flag
  | comment flag == Nothing = ""
  | otherwise               = "comment=" ++ (fromJust $ comment flag) ++ "&"

debugString :: Flag -> String
debugString flag
  | debug flag == True = "debug=true&"
  | otherwise          = "debug=false&"

referenceString :: Flag -> String
referenceString flag
  | reference flag == Nothing = ""
  | otherwise                 = "reference=" ++ (fromJust $ reference flag) ++ "&"
