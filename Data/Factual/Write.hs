-- | This module exports the definition of the Write typeclass.
module Data.Factual.Write
  (
  -- * Query typeclass
  Write(..)) where

-- | A member of the Write typeclass must define a url method which returns
--   the write url as a string and a body method which returns of the body
--   of the write.
class Write w where
  path :: w -> String
  body :: w -> String
