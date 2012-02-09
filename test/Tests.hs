import Test.HUnit
import Data.Factual.Query
import Data.Factual.Table
import Data.Factual.ReadQuery
import Data.Factual.SchemaQuery
import Data.Factual.ResolveQuery
import qualified Data.Factual.CrosswalkQuery as C

blankReadQuery :: ReadQuery
blankReadQuery = ReadQuery { table = Places
                           , search = AndSearch []
                           , select = []
                           , limit = Nothing
                           , offset = Nothing
                           , filters = []
                           , geo = Nothing
                           , includeCount = False }

blankCrosswalkQuery :: C.CrosswalkQuery
blankCrosswalkQuery = C.CrosswalkQuery { C.factualId = Nothing
                                       , C.limit = Nothing
                                       , C.namespace = Nothing
                                       , C.namespaceId = Nothing
                                       , C.only = [] }

placeTablePathTest = TestCase (do
  let expected = "/t/places/read?include_count=false"
  let path = toPath $ blankReadQuery { table = Places }
  assertEqual "Correct path for places table" expected path)

restaurantsTablePathTest = TestCase (do
  let expected = "/t/restaurants-us/read?include_count=false"
  let path = toPath $ blankReadQuery { table = USRestaurants }
  assertEqual "Correct path for us restaurants table" expected path)

globalTablePathTest = TestCase (do
  let expected = "/t/global/read?include_count=false"
  let path = toPath $ blankReadQuery { table = Global }
  assertEqual "Correct path for global table" expected path)

andSearchPathTest = TestCase (do
  let expected = "/t/places/read?q=foo bar&include_count=false"
  let path = toPath $ blankReadQuery { search = AndSearch ["foo", "bar"] }
  assertEqual "Correct path for ANDed search" expected path)

orSearchPathTest = TestCase (do
  let expected = "/t/places/read?q=foo,bar&include_count=false"
  let path = toPath $ blankReadQuery { search = OrSearch ["foo", "bar"] }
  assertEqual "Correct path for ANDed search" expected path)

selectPathTest = TestCase (do
  let expected = "/t/places/read?select=foo,bar&include_count=false"
  let path = toPath $ blankReadQuery { select = ["foo", "bar"] }
  assertEqual "Correct path for select terms" expected path)

limitPathTest = TestCase (do
  let expected = "/t/places/read?limit=321&include_count=false"
  let path = toPath $ blankReadQuery { limit = Just 321 }
  assertEqual "Correct path for limit" expected path)

offsetPathTest = TestCase (do
  let expected = "/t/places/read?offset=321&include_count=false"
  let path = toPath $ blankReadQuery { offset = Just 321 }
  assertEqual "Correct path for offset" expected path)

equalNumFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":123.4}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [EqualNum "field" 123.4] }
  assertEqual "Correct path for equal number filter" expected path)

equalStrFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":\"value\"}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [EqualStr "field" "value"] }
  assertEqual "Correct path for equal string filter" expected path)

notEqualNumFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":{\"$neq\":123.4}}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [NotEqualNum "field" 123.4] }
  assertEqual "Correct path for not equal number filter" expected path)

notEqualStrFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":{\"$neq\":\"value\"}}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [NotEqualStr "field" "value"] }
  assertEqual "Correct path for not equal string filter" expected path)

inNumListFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":{\"$in\":[123.4,5432.1]}}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [InNumList "field" [123.4, 5432.1]] }
  assertEqual "Correct path for in number list filter" expected path)

inStrListFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":{\"$in\":[\"value\",\"other\"]}}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [InStrList "field" ["value","other"]] }
  assertEqual "Correct path for in string list filter" expected path)

notInNumListFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":{\"$nin\":[123.4,5432.1]}}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [NotInNumList "field" [123.4, 5432.1]] }
  assertEqual "Correct path for not in number list filter" expected path)

notInStrListFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":{\"$nin\":[\"value\",\"other\"]}}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [NotInStrList "field" ["value","other"]] }
  assertEqual "Correct path for not in string list filter" expected path)

beginsWithFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":{\"$bw\":\"val\"}}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [BeginsWith "field" "val"] }
  assertEqual "Correct path for begins with filter" expected path)

notBeginsWithFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":{\"$nbw\":\"val\"}}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [NotBeginsWith "field" "val"] }
  assertEqual "Correct path for not begins with filter" expected path)

beginsWithAnyFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":{\"$bwin\":[\"val\",\"ot\"]}}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [BeginsWithAny "field" ["val","ot"]] }
  assertEqual "Correct path for begins with any filter" expected path)

notBeginsWithAnyFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":{\"$nbwin\":[\"val\",\"ot\"]}}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [NotBeginsWithAny "field" ["val","ot"]] }
  assertEqual "Correct path for not begins with any filter" expected path)

isBlankFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":{\"$blank\":true}}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [IsBlank "field"] }
  assertEqual "Correct path for is blank filter" expected path)

isNotBlankFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"field\":{\"$blank\":false}}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [IsNotBlank "field"] }
  assertEqual "Correct path for is not blank filter" expected path)

andFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"$and\":[{\"field1\":{\"$blank\":true}},{\"field2\":{\"$blank\":false}}]}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [And [IsBlank "field1", IsNotBlank "field2"]] }
  assertEqual "Correct path for and filter" expected path)

orFilterTest = TestCase (do
  let expected = "/t/places/read?filters={\"$or\":[{\"field1\":{\"$blank\":true}},{\"field2\":{\"$blank\":false}}]}&include_count=false"
  let path = toPath $ blankReadQuery { filters = [Or [IsBlank "field1", IsNotBlank "field2"]] }
  assertEqual "Correct path for or filter" expected path)

geoTest = TestCase (do
  let expected = "/t/places/read?geo={\"$circle\":{\"$center\":[300.1, 200.3],\"$meters\":100.5}}&include_count=false"
  let path = toPath $ blankReadQuery { geo = Just (Circle 300.1 200.3 100.5) }
  assertEqual "Correct path for geo" expected path)

includeCountTest = TestCase (do
  let expected = "/t/places/read?include_count=true"
  let path = toPath $ blankReadQuery { includeCount = True }
  assertEqual "Correct path for include count" expected path)

schemaQueryTest = TestCase (do
  let expected = "/t/places/schema"
  let path = toPath $ SchemaQuery Places
  assertEqual "Correct path for a schema query" expected path)

resolveQueryTest = TestCase (do
  let expected = "/places/resolve?values={\"field1\":\"value1\",\"field2\":32.1}"
  let path = toPath $ ResolveQuery [ResolveStr "field1" "value1", ResolveNum "field2" 32.1]
  assertEqual "Correct path for a resolve query" expected path)

factualIdTest = TestCase (do
  let expected = "/places/crosswalk?factual_id=1234"
  let path = toPath $ blankCrosswalkQuery { C.factualId = Just "1234" }
  assertEqual "Correct path for a factual id" expected path)

limitCWPathTest = TestCase (do
  let expected = "/places/crosswalk?limit=1234"
  let path = toPath $ blankCrosswalkQuery { C.limit = Just 1234 }
  assertEqual "Correct path for a limit in a crosswalk query" expected path)

namespaceTest = TestCase (do
  let expected = "/places/crosswalk?namespace=yelp"
  let path = toPath $ blankCrosswalkQuery { C.namespace = Just "yelp" }
  assertEqual "Correct path for a namespace" expected path)

namespaceIdTest = TestCase (do
  let expected = "/places/crosswalk?namespace_id=5432"
  let path = toPath $ blankCrosswalkQuery { C.namespaceId = Just "5432" }
  assertEqual "Correct path for a namespace id" expected path)

onlyTest = TestCase (do
  let expected = "/places/crosswalk?only=yelp,loopd"
  let path = toPath $ blankCrosswalkQuery { C.only = ["yelp", "loopd"] }
  assertEqual "Correct path for a only" expected path)


queryTests = TestList [ TestLabel "Place table test" placeTablePathTest
                 , TestLabel "Restaurants table test" restaurantsTablePathTest
                 , TestLabel "Global table test" globalTablePathTest
                 , TestLabel "And search test" andSearchPathTest
                 , TestLabel "Or search test" orSearchPathTest
                 , TestLabel "Select test" selectPathTest
                 , TestLabel "Limit test" limitPathTest
                 , TestLabel "Offset test" offsetPathTest
                 , TestLabel "Equal number filter test" equalNumFilterTest
                 , TestLabel "Equal string filter test" equalStrFilterTest
                 , TestLabel "Not equal number filter test" notEqualNumFilterTest
                 , TestLabel "Not equal string filter test" notEqualStrFilterTest
                 , TestLabel "In number list filter test" inNumListFilterTest
                 , TestLabel "In string list filter test" inStrListFilterTest
                 , TestLabel "Not in number list filter test" notInNumListFilterTest
                 , TestLabel "Not in string list filter test" notInStrListFilterTest
                 , TestLabel "Begins with filter test" beginsWithFilterTest
                 , TestLabel "Not begins with filter test" notBeginsWithFilterTest
                 , TestLabel "Begins with any filter test" beginsWithAnyFilterTest
                 , TestLabel "Not begins with any filter test" notBeginsWithAnyFilterTest
                 , TestLabel "Is blank filter test" isBlankFilterTest
                 , TestLabel "Is not blank filter test" isNotBlankFilterTest
                 , TestLabel "And filter test" andFilterTest
                 , TestLabel "Or filter test" andFilterTest
                 , TestLabel "Geo test" geoTest
                 , TestLabel "Include count test" includeCountTest
                 , TestLabel "Schema query test" schemaQueryTest
                 , TestLabel "Resolve query test" resolveQueryTest
                 , TestLabel "Factual ID test" factualIdTest
                 , TestLabel "Crosswalk limit test" limitCWPathTest
                 , TestLabel "Namespace test" namespaceTest
                 , TestLabel "Namespace ID test" namespaceIdTest
                 , TestLabel "Only test" onlyTest ]

runQueryTests = runTestTT queryTests
