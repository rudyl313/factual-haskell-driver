import Test.HUnit
import Data.Factual.Query
import Data.Factual.Table
import Data.Factual.ReadQuery

placeTableTest = TestCase (do
  let expected = "/t/places/read?include_count=false"
  assertEqual "Correct path for places table" (pathForTable Places) expected)

restaurantsTableTest = TestCase (do
  let expected = "/t/restaurants-us/read?include_count=false"
  assertEqual "Correct path for us restaurants table" (pathForTable USRestaurants) expected)

globalTableTest = TestCase (do
  let expected = "/t/global/read?include_count=false"
  assertEqual "Correct path for global table" (pathForTable Global) expected)

pathForTable :: Table -> String
pathForTable = toPath . readQueryForTable

readQueryForTable :: Table -> ReadQuery
readQueryForTable t = blankReadQuery { table = t }

blankReadQuery :: ReadQuery
blankReadQuery = ReadQuery { table = Places
                           , search = AndSearch []
                           , select = []
                           , limit = Nothing
                           , offset = Nothing
                           , filters = []
                           , geo = Nothing
                           , includeCount = False }

tests = TestList [ TestLabel "Place table test" placeTableTest
                 , TestLabel "Restaurants table test" restaurantsTableTest
                 , TestLabel "Global table test" globalTableTest ]

main = runTestTT tests
