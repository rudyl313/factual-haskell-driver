import Test.HUnit
import Data.Factual.Query
import Data.Factual.Table
import Data.Factual.ReadQuery

tests = TestList [ TestLabel "Table test" table_test ]

table_test = TestCase (do let query = ReadQuery { table = Places
                                                , search = AndSearch []
                                                , select = []
                                                , limit = Nothing
                                                , offset = Nothing
                                                , filters = []
                                                , geo = Nothing
                                                , includeCount = False }
                          let path = toPath query
                          assertEqual "Correct table" path "/t/places/read?include_count=false")

main = runTestTT tests
