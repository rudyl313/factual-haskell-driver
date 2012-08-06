-- | This module exports the types used to create diffs queries.
module Data.Factual.Query.DiffsQuery
 (
   -- * DiffsQuery type
   DiffsQuery(..)
   -- * Required modules
 , module Data.Factual.Shared.Table
 ) where

import Data.Factual.Query
import Data.Factual.Shared.Table
import qualified Data.Map as M

-- | The timestamps are unix epoch integers
type Timestamp = Integer

-- | The DiffsQuery type is used to construct diffs queries. A table, start
--   timestamp and end timestamp should be specified.
data DiffsQuery = DiffsQuery { table :: Table
                             , start :: Timestamp
                             , end   :: Timestamp
                             } deriving (Eq, Show)

-- The DiffsQuery type is a member of the Query typeclass so it can be used to
-- make a request.
instance Query DiffsQuery where
  path   query = (show $ table query) ++ "/diffs"
  params query = M.fromList [ ("start", show $ start query)
                            , ("end", show $ end query) ]
