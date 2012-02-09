-- | This module contains a simple definition of the credentials type which is
--   used to perform submit queries to the Factual API.

module Data.Factual.Credentials
  (
    -- * Credentials type
    Credentials(..)
  , Key
  , Secret
  ) where

-- | The Key is the oauth key as a String.
type Key = String
-- | The Secret is the oauth secret as a String.
type Secret = String

-- | A Credentials value is used to generate a Token.
data Credentials = Credentials Key Secret
