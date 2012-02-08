import Test.HUnit
import Data.Factual.Query
import Data.Factual.Table
import Data.Factual.ReadQuery

placeTablePathTest = TestCase (do
  let expected = "/t/places/read?include_count=false"
  assertEqual "Correct path for places table" (pathForTable Places) expected)

restaurantsTablePathTest = TestCase (do
  let expected = "/t/restaurants-us/read?include_count=false"
  assertEqual "Correct path for us restaurants table" (pathForTable USRestaurants) expected)

globalTablePathTest = TestCase (do
  let expected = "/t/global/read?include_count=false"
  assertEqual "Correct path for global table" (pathForTable Global) expected)

andSearchPathTest = TestCase (do
  let expected = "/t/places/read?q=foo bar&include_count=false"
  let path = toPath $ blankReadQuery { search = AndSearch ["foo", "bar"] }
  assertEqual "Correct path for ANDed search" path expected)

orSearchPathTest = TestCase (do
  let expected = "/t/places/read?q=foo,bar&include_count=false"
  let path = toPath $ blankReadQuery { search = OrSearch ["foo", "bar"] }
  assertEqual "Correct path for ANDed search" path expected)

selectPathTest = TestCase (do
  let expected = "/t/places/read?select=foo,bar&include_count=false"
  let path = toPath $ blankReadQuery { select = ["foo", "bar"] }
  assertEqual "Correct path for select terms" path expected)

limitPathTest = TestCase (do
  let expected = "/t/places/read?limit=321&include_count=false"
  let path = toPath $ blankReadQuery { limit = Just 321 }
  assertEqual "Correct path for limit" path expected)

offsetPathTest = TestCase (do
  let expected = "/t/places/read?offset=321&include_count=false"
  let path = toPath $ blankReadQuery { offset = Just 321 }
  assertEqual "Correct path for offset" path expected)

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

tests = TestList [ TestLabel "Place table test" placeTablePathTest
                 , TestLabel "Restaurants table test" restaurantsTablePathTest
                 , TestLabel "Global table test" globalTablePathTest
                 , TestLabel "And search test" andSearchPathTest
                 , TestLabel "Or search test" orSearchPathTest
                 , TestLabel "Select test" selectPathTest
                 , TestLabel "Limit test" limitPathTest
                 , TestLabel "Offset test" offsetPathTest ]

main = runTestTT tests
