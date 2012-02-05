module Data.Factual.Circle (Circle(..)) where

type Lat = Double
type Long = Double
type Radius = Double

data Circle = Circle Lat Long Radius deriving Eq

instance Show Circle where
  show (Circle lat long radius) = "{\"$circle\":{\"$center\":[" 
                                ++ (show lat) 
                                ++ ", "
                                ++ (show long)
                                ++ "],\"$meters\":"
                                ++ (show radius)
                                ++ "}}"

