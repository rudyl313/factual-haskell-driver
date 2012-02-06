import Test.HUnit
import Data.Factual.Query
import Data.Factual.Table
import Data.Factual.ReadQuery

placeTableTest = TestCase (do let query = blankReadQuery { table = Places }
                              let path = toPath query
                              assertEqual "Correct table" path "/t/places/read?include_count=false")

restaurantsTableTest = TestCase (do let query = blankReadQuery { table = USRestaurants }
                                    let path = toPath query
                                    assertEqual "Correct table" path "/t/restaurants-us/read?include_count=false")

globalTableTest = TestCase (do let query = blankReadQuery { table = Global }
                               let path = toPath query
                               assertEqual "Correct table" path "/t/global/read?include_count=false")

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
