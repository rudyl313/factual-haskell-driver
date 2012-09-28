-- | This module exports the types used to create flag writes.
module Data.Factual.Write.Flag
  (
    -- * Flag type
    Flag(..)
    -- * Problem type
  , Problem(..)
    -- * Required modules
  , module Data.Factual.Shared.Table
  ) where

import Data.Factual.Write
import Data.Factual.Shared.Table
import Data.Maybe (fromJust)
import Data.List.Utils (join)
import Data.Factual.Utils
import qualified Data.Map as M

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
                 , dataJSON  :: Maybe String
                 , fields    :: Maybe [String]
                 , reference :: Maybe String
                 } deriving (Eq, Show)

-- The Flag type is a member of the Write typeclass so it can be sent as a post
-- request to the API.
instance Write Flag where
  path flag = (show $ table flag) ++ "/" ++ (factualId flag) ++ "/flag"
  params _  = M.empty
  body flag = M.fromList [ ("problem", show $ problem flag)
                         , ("user", user flag)
                         , commentPair flag
                         , dataPair flag
                         , fieldsPair flag
                         , referencePair flag ]

-- The following functions are helpers for the body function
commentPair :: Flag -> (String, String)
commentPair flag
  | comment flag == Nothing = ("comment", "")
  | otherwise               = ("comment", fromJust $ comment flag)

dataPair :: Flag -> (String, String)
dataPair flag
  | dataJSON flag == Nothing = ("data", "")
  | otherwise                = ("data", fromJust $ dataJSON flag)

fieldsPair :: Flag -> (String, String)
fieldsPair flag
  | fields flag == Nothing = ("fields", "")
  | otherwise              = ("fields", arrayString)
  where arrayString = "[" ++ (join "," $ fromJust $ fields flag) ++ "]"

referencePair :: Flag -> (String, String)
referencePair flag
  | reference flag == Nothing = ("reference", "")
  | otherwise                 = ("reference", fromJust $ reference flag)
