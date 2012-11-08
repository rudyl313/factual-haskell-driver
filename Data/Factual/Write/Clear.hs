-- | This module exports the types used to clear fields.
module Data.Factual.Write.Clear
  (
    -- * Clear type
    Clear(..)
    -- * Required modules
  , module Data.Factual.Shared.Table
  ) where

import Data.Factual.Write
import Data.Factual.Shared.Table
import Data.List.Utils (join)
import qualified Data.Map as M

-- | The Clear type represents a Write to be made to the API which will clear
--   certain fields from an entry. The table and factualId identify the row to
--   be changed, and the fields list indicates which fields to clear out. A user
--   must be specified as well.
data Clear = Clear { table     :: Table
                   , factualId :: String
                   , fields    :: [String]
                   , user      :: String
                   } deriving (Eq, Show)

-- The Clear type is a member of the Write typeclass so it can be sent as a post
-- request to the API.
instance Write Clear where
  path clear = (show $ table clear) ++ "/" ++ (factualId clear) ++ "/clear"
  params _  = M.empty
  body clear = M.fromList [ ("user", user clear)
                          , ("fields", join "," $ fields clear) ]
