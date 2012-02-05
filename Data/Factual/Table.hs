module Data.Factual.Table (Table(..)) where

data Table = Places | USRestaurants | Global deriving Eq

instance Show Table where
  show Places = "/t/places/"
  show USRestaurants = "/t/restaurants-us/"
  show Global = "/t/global/"
