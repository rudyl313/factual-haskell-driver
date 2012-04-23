-- | This module exports the Geo type used to create read and facet queries.
module Data.Factual.Geo
  (
    -- * Geo type
    Lat
  , Long
  , Radius
  , Geo(..)
    -- * Helper functions
  , geoString
  ) where

-- | A Lat is the latitude represented as a Double.
type Lat = Double
-- | A Long is the longitude represented as a Double.
type Long = Double
-- | A Radius is the radius of the circle as a Double in meters.
type Radius = Double

-- | The Geo type is used to limit the search to specific geograph location.
--   Currently, only circles are supported. Supply a latitude, longitude and
--   radius in meters for the circle.
data Geo = Circle Lat Long Radius deriving Eq

-- Geo is a member of Show to help generate query strings.
instance Show Geo where
  show (Circle lat long radius) = "{\"$circle\":{\"$center\":[" 
                                ++ (show lat) 
                                ++ ", "
                                ++ (show long)
                                ++ "],\"$meters\":"
                                ++ (show radius)
                                ++ "}}"

-- Helper functions
geoString :: Maybe Geo -> String
geoString (Just c) = "geo=" ++ show c
geoString Nothing = ""
