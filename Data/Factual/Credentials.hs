-- | This module contains a simple definition of the credentials type which is
--   used to perform submit queries to the Factual API.

module Data.Factual.Credentials
  (
    -- * Credentials type
    Credentials(..)
  ) where

type Key = String
type Secret = String

-- | Both Key and Secret are aliases for String and represent the oauth key
--   and secret used to access the API.
data Credentials = Credentials Key Secret
