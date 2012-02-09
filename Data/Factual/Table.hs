-- | This module exports the type used to represent a table for the read or
--   schema query types.
module Data.Factual.Table
  (
     -- * Table type
     Table(..)
  ) where

-- | Three Table values are currently available for the Factual API.
data Table = Places | USRestaurants | Global deriving Eq

-- Table is a member of the Show typeclass to generate the beginning of the path.
instance Show Table where
  show Places = "/t/places/"
  show USRestaurants = "/t/restaurants-us/"
  show Global = "/t/global/"
