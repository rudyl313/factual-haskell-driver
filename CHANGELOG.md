## 0.5.0 (August 8, 2012)
  - Modified Raw Read function (get) to take a path and a Map of query
    parameters instead of just a query string
  - Added Raw Write function (post) to take a path, query param Map, and
    a body contents Map
  - Renamed the query and write API functions to executeQuery and
    executeWrite accordingly
  - Added the MatchQuery type
  - Added the sort field for Read queries
  - Added more filters for queries that support filters: GreaterThan,
    GreaterThanOrEqualTo, LessThan, LessThanOrEqualTo, and SearchFilter

## 0.4.0 (July 9, 2012)
  - Removed deprecated Crosswalk endpoint (from now on it should be a
    normal Read query using the Crosswalk table)

## 0.3.3 (June 27, 2012)
  - Added debug functions to inspect query strings and/or body contents

## 0.3.2 (June 20, 2012)
  - Updated tables list: RestaurantsUS (previously USRestaurants) and HotelsUS

## 0.3.1 (June 13, 2012)
  - Removed deprecated read action from the ReadQuery path strings

## 0.3.0 (June 5, 2012)
  - Migrated to GHC 7.4.1 and Haskell Platform 2012.2.0.0
  - Added multi call support
  - Improved error handling with additional fields added to the Response
    type
  - Added more Table values: HealthCareProviders, WorldGeographies,
    ProductsCPG, and ProductsCrosswalk

## 0.2.0 (May 23, 2012)
  - Removed Credentials type and replaced functionality with just
    passing the Key and Secret directly to the Token creator
  - Added FacetsQuery
  - Added GeocodeQuery
  - Added GeopulseQuery
  - Added Flag
  - Added Submit
  - Added RawRequest feature
  - Reduced the number of imports required to make basic queries

## 0.1.2 (Feb 10, 2012)
  - Added X-Factual-Lib header to requests

## 0.1.1 (Feb 9, 2012)
  - Minor changes to README, LICENSE and cabal file

## 0.1.0 (Feb 9, 2012)
  - Initial release
