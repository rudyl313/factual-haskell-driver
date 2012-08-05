-- | This module exports the definition of the Write typeclass.
module Data.Factual.Write
  (
  -- * Write typeclass
  Write(..)) where

import qualified Data.Map as M

-- | A member of the Write typeclass must define a path function which returns
--   the write path as a String, a params function that outputs any addition path
--   params as a Map, and a body function which returns Map of the data passed in
--   the body of the post request.
class Write w where
  path   :: w -> String
  params :: w -> M.Map String String
  body   :: w -> M.Map String String
