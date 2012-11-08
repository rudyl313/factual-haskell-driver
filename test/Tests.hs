import Test.HUnit
import System.IO
import System.Environment
import Data.String.Utils
import Network.Factual.API
import Data.Factual.Query.ReadQuery
import Data.Factual.Query.SchemaQuery
import Data.Factual.Query.ResolveQuery
import Data.Factual.Query.GeocodeQuery
import Data.Factual.Query.MatchQuery
import Data.Factual.Response
import qualified Data.Factual.Query.DiffsQuery as D
import qualified Data.Factual.Query as Q
import qualified Data.Map as M
import qualified Data.Factual.Write as W
import qualified Data.Factual.Query.FacetsQuery as F
import qualified Data.Factual.Query.GeopulseQuery as G
import qualified Data.Factual.Write.Submit as S
import qualified Data.Factual.Write.Insert as I
import qualified Data.Factual.Write.Flag as L
import qualified Data.Factual.Write.Clear as C

runUnitTests = runTestTT unitTests

runIntegrationTests = do
  token <- getTokenFromCredentialsFile
  runTestTT $ integrationTests token

getTokenFromCredentialsFile = do
  home <- getEnv "HOME"
  yaml <- readFile $ home ++ "/.factual/factual-auth.yaml"
  let lines = split "\n" yaml
  let lines' = filter (\x -> length (split ":" x) == 2) lines
  let pairs = map (map strip . split ":") lines'
  let key = last $ head $ filter (\x -> head x == "key") pairs
  let secret = last $ head $ filter (\x -> head x == "secret") pairs
  let token = generateToken key secret
  return token

unitTests = TestList [ TestLabel "Place table test" placeTablePathTest
                     , TestLabel "Restaurants table test" restaurantsTablePathTest
                     , TestLabel "Hotels table test" hotelsTablePathTest
                     , TestLabel "Global table test" globalTablePathTest
                     , TestLabel "Crosswalk table test" crosswalkProductsTablePathTest
                     , TestLabel "Healthcare table test" healthcareTablePathTest
                     , TestLabel "World Geographies table test" worldGeographiesTablePathTest
                     , TestLabel "CPG table test" cpgTablePathTest
                     , TestLabel "Crosswalk Products table test" crosswalkProductsTablePathTest
                     , TestLabel "Monetize table test" monetizeTablePathTest
                     , TestLabel "Custom table test" customTablePathTest
                     , TestLabel "Geopulse test" geopulseTest
                     , TestLabel "Geocode test" geocodeTest
                     , TestLabel "And search test" andSearchPathTest
                     , TestLabel "Or search test" orSearchPathTest
                     , TestLabel "Select test" selectTest
                     , TestLabel "Limit test" limitTest
                     , TestLabel "Offset test" offsetTest
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
                     , TestLabel "Greater than number filter test" greaterThanFilterTest
                     , TestLabel "Greater than or equal to number filter test" greaterThanOrEqualToFilterTest
                     , TestLabel "Less than number filter test" lessThanFilterTest
                     , TestLabel "Less than or equal to number filter test" lessThanOrEqualToFilterTest
                     , TestLabel "Search filter test" searchFilterTest
                     , TestLabel "And filter test" andFilterTest
                     , TestLabel "Or filter test" orFilterTest
                     , TestLabel "Geo test" geoTest
                     , TestLabel "Sort test" sortTest
                     , TestLabel "Include count test" includeCountTest
                     , TestLabel "Schema query test" schemaQueryTest
                     , TestLabel "Resolve query test" resolveQueryTest
                     , TestLabel "Match query test" matchQueryTest
                     , TestLabel "Facets test" facetsTest
                     , TestLabel "Diffs test" diffsTest
                     , TestLabel "Submit path test" submitPathTest
                     , TestLabel "Submit body test" submitBodyTest
                     , TestLabel "Insert path test" insertPathTest
                     , TestLabel "Insert body test" insertBodyTest
                     , TestLabel "Flag path test" flagPathTest
                     , TestLabel "Flag body test" flagBodyTest
                     , TestLabel "Clear path test" clearPathTest
                     , TestLabel "Clear body test" clearBodyTest ]

integrationTests token = TestList [ TestLabel "Read test" (readIntegrationTest token)
                                  , TestLabel "Unicode test" (unicodeIntegrationTest token)
                                  , TestLabel "Schema test" (schemaIntegrationTest token)
                                  , TestLabel "Resolve test" (resolveIntegrationTest token)
                                  , TestLabel "Match test" (matchIntegrationTest token)
                                  , TestLabel "Raw read test" (rawIntegrationTest token)
                                  , TestLabel "Facets test" (facetsIntegrationTest token)
                                  --, TestLabel "Diffs test" (diffsIntegrationTest token)
                                  , TestLabel "Geopulse test" (geopulseIntegrationTest token)
                                  , TestLabel "Geocode test" (geocodeIntegrationTest token)
                                  , TestLabel "Multi test" (multiIntegrationTest token)
                                  , TestLabel "Submit test" (submitIntegrationTest token)
                                  , TestLabel "Insert test" (insertIntegrationTest token)
                                  , TestLabel "Flag test" (flagIntegrationTest token)
                                  , TestLabel "Clear test" (clearIntegrationTest token)
                                  , TestLabel "Error test" (errorIntegrationTest token) ]

placeTablePathTest = TestCase (do
  let expected = "/t/places"
  let queryPath = Q.path $ blankReadQuery { table = Places }
  assertEqual "Correct path for places table" expected queryPath)

restaurantsTablePathTest = TestCase (do
  let expected = "/t/restaurants-us"
  let queryPath = Q.path $ blankReadQuery { table = RestaurantsUS }
  assertEqual "Correct path for us restaurants table" expected queryPath)

hotelsTablePathTest = TestCase (do
  let expected = "/t/hotels-us"
  let queryPath = Q.path $ blankReadQuery { table = HotelsUS }
  assertEqual "Correct path for us hotels table" expected queryPath)

globalTablePathTest = TestCase (do
  let expected = "/t/global"
  let queryPath = Q.path $ blankReadQuery { table = Global }
  assertEqual "Correct path for global table" expected queryPath)

crosswalkTablePathTest = TestCase (do
  let expected = "/t/crosswalk"
  let queryPath = Q.path $ blankReadQuery { table = Crosswalk }
  assertEqual "Correct path for crosswalk table" expected queryPath)

healthcareTablePathTest = TestCase (do
  let expected = "/t/health-care-providers-us"
  let queryPath = Q.path $ blankReadQuery { table = HealthCareProviders }
  assertEqual "Correct path for health care providers table" expected queryPath)

worldGeographiesTablePathTest = TestCase (do
  let expected = "/t/world-geographies"
  let queryPath = Q.path $ blankReadQuery { table = WorldGeographies }
  assertEqual "Correct path for world geographies table" expected queryPath)

cpgTablePathTest = TestCase (do
  let expected = "/t/products-cpg"
  let queryPath = Q.path $ blankReadQuery { table = ProductsCPG }
  assertEqual "Correct path for products CPG table" expected queryPath)

crosswalkProductsTablePathTest = TestCase (do
  let expected = "/t/products-crosswalk"
  let queryPath = Q.path $ blankReadQuery { table = ProductsCrosswalk }
  assertEqual "Correct path for products crosswalk table" expected queryPath)

monetizeTablePathTest = TestCase (do
  let expected = "/places/monetize"
  let queryPath = Q.path $ blankReadQuery { table = Monetize }
  assertEqual "Correct path for monetize table" expected queryPath)

customTablePathTest = TestCase (do
  let expected = "/t/foo"
  let queryPath = Q.path $ blankReadQuery { table = Custom "foo" }
  assertEqual "Correct path for custom table" expected queryPath)

geopulseTest = TestCase (do
  let query = G.GeopulseQuery { G.geo = Point 34.06021 (-118.41828) , G.select = ["commercial_density"] }
  let queryPath = Q.path query
  let queryParams = Q.params query
  assertEqual "Correct path for geopulse" queryPath "/places/geopulse"
  assertEqual "Correct geo param" (queryParams M.! "geo") "{\"$point\":[34.06021,-118.41828]}"
  assertEqual "Correct select param" (queryParams M.! "select") "commercial_density")

geocodeTest = TestCase (do
  let query = GeocodeQuery $ Point 34.06021 (-118.41828)
  let queryPath = Q.path query
  let queryParams = Q.params query
  assertEqual "Correct path for geocode" queryPath "/places/geocode"
  assertEqual "Correct geo param" (queryParams M.! "geo") "{\"$point\":[34.06021,-118.41828]}")

andSearchPathTest = TestCase (do
  let query = blankReadQuery { search = AndSearch ["foo", "bar"] }
  let queryParams = Q.params query
  assertEqual "Correct query value" (queryParams M.! "q")  "foo bar")

orSearchPathTest = TestCase (do
  let query = blankReadQuery { search = OrSearch ["foo", "bar"] }
  let queryParams = Q.params query
  assertEqual "Correct query value" (queryParams M.! "q")  "foo,bar")

selectTest = TestCase (do
  let query = blankReadQuery { select = ["foo", "bar"] }
  let queryParams = Q.params query
  assertEqual "Correct select value" (queryParams M.! "select")  "foo,bar")

limitTest = TestCase (do
  let query = blankReadQuery { limit = Just 321 }
  let queryParams = Q.params query
  assertEqual "Correct limit value" (queryParams M.! "limit") "321")

offsetTest = TestCase (do
  let query = blankReadQuery { offset = Just 321 }
  let queryParams = Q.params query
  assertEqual "Correct offset value" (queryParams M.! "offset") "321")

equalNumFilterTest = TestCase (do
  let query = blankReadQuery { filters = [EqualNum "field" 123.4] }
  let queryParams = Q.params query
  assertEqual "Correct filters value" (queryParams M.! "filters") "{\"field\":123.4}")

equalStrFilterTest = TestCase (do
  let query = blankReadQuery { filters = [EqualStr "field" "value"] }
  let queryParams = Q.params query
  assertEqual "Correct filters value" (queryParams M.! "filters") "{\"field\":\"value\"}")

notEqualNumFilterTest = TestCase (do
  let query = blankReadQuery { filters = [NotEqualNum "field" 123.4] }
  let queryParams = Q.params query
  assertEqual "Correct filters value" (queryParams M.! "filters") "{\"field\":{\"$neq\":123.4}}")

notEqualStrFilterTest = TestCase (do
  let query = blankReadQuery { filters = [NotEqualStr "field" "value"] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$neq\":\"value\"}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

inNumListFilterTest = TestCase (do
  let query = blankReadQuery { filters = [InNumList "field" [123.4, 5432.1]] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$in\":[123.4,5432.1]}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

inStrListFilterTest = TestCase (do
  let query = blankReadQuery { filters = [InStrList "field" ["value","other"]] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$in\":[\"value\",\"other\"]}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

notInNumListFilterTest = TestCase (do
  let query = blankReadQuery { filters = [NotInNumList "field" [123.4, 5432.1]] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$nin\":[123.4,5432.1]}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

notInStrListFilterTest = TestCase (do
  let query = blankReadQuery { filters = [NotInStrList "field" ["value","other"]] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$nin\":[\"value\",\"other\"]}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

beginsWithFilterTest = TestCase (do
  let query = blankReadQuery { filters = [BeginsWith "field" "val"] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$bw\":\"val\"}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

notBeginsWithFilterTest = TestCase (do
  let query = blankReadQuery { filters = [NotBeginsWith "field" "val"] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$nbw\":\"val\"}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

beginsWithAnyFilterTest = TestCase (do
  let query = blankReadQuery { filters = [BeginsWithAny "field" ["val","ot"]] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$bwin\":[\"val\",\"ot\"]}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

notBeginsWithAnyFilterTest = TestCase (do
  let query = blankReadQuery { filters = [NotBeginsWithAny "field" ["val","ot"]] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$nbwin\":[\"val\",\"ot\"]}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

isBlankFilterTest = TestCase (do
  let query = blankReadQuery { filters = [IsBlank "field"] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$blank\":true}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

isNotBlankFilterTest = TestCase (do
  let query = blankReadQuery { filters = [IsNotBlank "field"] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$blank\":false}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

greaterThanFilterTest = TestCase (do
  let query = blankReadQuery { filters = [GreaterThan "field" 10.0] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$gt\":10.0}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

greaterThanOrEqualToFilterTest = TestCase (do
  let query = blankReadQuery { filters = [GreaterThanOrEqualTo "field" 10.0] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$gte\":10.0}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

lessThanFilterTest = TestCase (do
  let query = blankReadQuery { filters = [LessThan "field" 10.0] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$lt\":10.0}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

lessThanOrEqualToFilterTest = TestCase (do
  let query = blankReadQuery { filters = [LessThanOrEqualTo "field" 10.0] }
  let queryParams = Q.params query
  let expected = "{\"field\":{\"$lte\":10.0}}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

searchFilterTest = TestCase (do
  let query = blankReadQuery { filters = [SearchFilter "field" "value"] }
  let queryParams = Q.params query
  assertEqual "Correct filters value" (queryParams M.! "filters") "{\"field\":{\"$search\":\"value\"}}")

andFilterTest = TestCase (do
  let query = blankReadQuery { filters = [And [IsBlank "field1", IsNotBlank "field2"]] }
  let queryParams = Q.params query
  let expected = "{\"$and\":[{\"field1\":{\"$blank\":true}},{\"field2\":{\"$blank\":false}}]}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

orFilterTest = TestCase (do
  let query = blankReadQuery { filters = [Or [IsBlank "field1", IsNotBlank "field2"]] }
  let queryParams = Q.params query
  let expected = "{\"$or\":[{\"field1\":{\"$blank\":true}},{\"field2\":{\"$blank\":false}}]}"
  assertEqual "Correct filters value" (queryParams M.! "filters") expected)

geoTest = TestCase (do
  let query = blankReadQuery { geo = Just (Circle 300.1 200.3 100.5) }
  let queryParams = Q.params query
  let expected = "{\"$circle\":{\"$center\":[300.1, 200.3],\"$meters\":100.5}}"
  assertEqual "Correct geo value" (queryParams M.! "geo") expected)

sortTest = TestCase (do
  let query = blankReadQuery { sort = [Asc "name", Desc "country"] }
  let queryParams = Q.params query
  let expected = "name:asc,country:desc"
  assertEqual "Correct sort value" (queryParams M.! "sort") expected)

includeCountTest = TestCase (do
  let query = blankReadQuery { includeCount = True }
  let queryParams = Q.params query
  assertEqual "Correct include_count value" (queryParams M.! "include_count") "true")

schemaQueryTest = TestCase (do
  let queryPath = Q.path $ SchemaQuery Places
  let expected = "/t/places/schema"
  assertEqual "Correct path for a schema query" queryPath expected)

resolveQueryTest = TestCase (do
  let query = ResolveQuery { values = [ResolveStr "field1" "value1", ResolveNum "field2" 32.1],
                             debug  = False }
  let queryPath = Q.path query
  let queryParams = Q.params query
  let expectedValues = "{\"field1\":\"value1\",\"field2\":32.1}"
  assertEqual "Correct path" queryPath "/places/resolve"
  assertEqual "Correct values" (queryParams M.! "values") expectedValues
  assertEqual "Correct debug" (queryParams M.! "debug") "false")

matchQueryTest = TestCase (do
  let query = MatchQuery [MatchStr "field1" "value1", MatchNum "field2" 32.1]
  let queryPath = Q.path query
  let queryParams = Q.params query
  let expectedValues = "{\"field1\":\"value1\",\"field2\":32.1}"
  assertEqual "Correct path" queryPath "/places/match"
  assertEqual "Correct values" (queryParams M.! "values") expectedValues)

facetsTest = TestCase (do
  let query = F.FacetsQuery { F.table        = Places
                            , F.search       = AndSearch ["starbucks"]
                            , F.select       = ["locality", "region"]
                            , F.filters      = [EqualStr "country" "US"]
                            , F.geo          = Nothing
                            , F.limit        = Just 10
                            , F.minCount     = Just 2
                            , F.includeCount = False }
  let queryPath = Q.path query
  let queryParams = Q.params query
  assertEqual "Correct path for a facets query" queryPath "/t/places/facets"
  assertEqual "Correct q" (queryParams M.! "q") "starbucks"
  assertEqual "Correct select" (queryParams M.! "select") "locality,region"
  assertEqual "Correct filters" (queryParams M.! "filters") "{\"country\":\"US\"}"
  assertEqual "Correct limit" (queryParams M.! "limit") "10"
  assertEqual "Correct min count" (queryParams M.! "min_count") "2"
  assertEqual "Correct include count" (queryParams M.! "include_count") "false")

diffsTest = TestCase (do
  let query = D.DiffsQuery { D.table = Places, D.start = 1318890505254, D.end = 1318890516892 }
  let queryPath = Q.path query
  let queryParams = Q.params query
  assertEqual "Correct path for a diffs query" queryPath "/t/places/diffs"
  assertEqual "Correct start" (queryParams M.! "start") "1318890505254"
  assertEqual "Correct end" (queryParams M.! "end") "1318890516892")

submitPathTest = TestCase (do
  let expected = "/t/places/foobar/submit"
  let writePath = W.path submitWrite
  assertEqual "Correct path for submit" expected writePath)

submitBodyTest = TestCase (do
  let expected = "user=user123&values={\"key\":\"val\"}"
  let bodyParams = W.body submitWrite
  assertEqual "Correct user" (bodyParams M.! "user") "user123"
  assertEqual "Correct values" (bodyParams M.! "values") "{\"key\":\"val\"}")

insertPathTest = TestCase (do
  let expected = "/t/places/foobar/insert"
  let writePath = W.path insertWrite
  assertEqual "Correct path for insert" expected writePath)

insertBodyTest = TestCase (do
  let expected = "user=user123&values={\"key\":\"val\"}"
  let bodyParams = W.body insertWrite
  assertEqual "Correct user" (bodyParams M.! "user") "user123"
  assertEqual "Correct values" (bodyParams M.! "values") "{\"key\":\"val\"}")

flagPathTest = TestCase (do
  let expected = "/t/places/foobar/flag"
  let path = W.path flagWrite
  assertEqual "Correct path for flag" expected path)

flagBodyTest = TestCase (do
  let bodyParams = W.body flagWrite
  assertEqual "Correct problem" (bodyParams M.! "problem") "Duplicate"
  assertEqual "Correct user" (bodyParams M.! "user") "user123"
  assertEqual "Correct comment" (bodyParams M.! "comment") "There was a problem")

clearPathTest = TestCase (do
  let expected = "/t/places/foobar/clear"
  let path = W.path clearWrite
  assertEqual "Correct path for clear" expected path)

clearBodyTest = TestCase (do
  let bodyParams = W.body clearWrite
  assertEqual "Correct user" (bodyParams M.! "user") "user123"
  assertEqual "Correct fields" (bodyParams M.! "fields") "latitude,longitude")

readIntegrationTest :: Token -> Test
readIntegrationTest token = TestCase (do
  let query = ReadQuery { table = Places
                        , search = AndSearch ["McDonalds", "Burger King"]
                        , select = ["name"]
                        , limit = Just 50
                        , offset = Just 10
                        , includeCount = True
                        , geo = Just (Circle 34.06021 (-118.41828) 5000.0)
                        , sort = []
                        , filters = [EqualStr "name" "Stand"] }
  result <- executeQuery (Options { token = token, timeout = Nothing }) query
  assertEqual "Valid read query" "ok" (status result))

unicodeIntegrationTest :: Token -> Test
unicodeIntegrationTest token = TestCase (do
  let query = ReadQuery { table = Global
                        , search = NoSearch
                        , select = []
                        , limit = Nothing
                        , offset = Nothing
                        , includeCount = False
                        , geo = Nothing
                        , sort = []
                        , filters = [EqualStr "locality" "בני ברק"] }
  result <- executeQuery (Options { token = token, timeout = Nothing }) query
  let resp = response result
  let dat = lookupValue "data" resp
  let row = (toList dat) !! 0
  let loc = lookupString "locality" row
  assertEqual "Correctly encoded locality" "בני ברק" loc)

schemaIntegrationTest :: Token -> Test
schemaIntegrationTest token = TestCase (do
  let query = SchemaQuery Places
  result <- executeQuery (Options { token = token, timeout = Nothing }) query
  assertEqual "Valid schema query" "ok" (status result))

resolveIntegrationTest :: Token -> Test
resolveIntegrationTest token = TestCase (do
  let query = ResolveQuery { values = [ResolveStr "name" "McDonalds"],
                             debug = False }
  result <- executeQuery (Options { token = token, timeout = Nothing }) query
  assertEqual "Valid resolve query" "ok" (status result))

matchIntegrationTest :: Token -> Test
matchIntegrationTest token = TestCase (do
  let query = MatchQuery [MatchStr "name" "McDonalds"]
  result <- executeQuery (Options { token = token, timeout = Nothing }) query
  assertEqual "Valid match query" "ok" (status result))

rawIntegrationTest :: Token -> Test
rawIntegrationTest token = TestCase (do
  result <- get (Options { token = token, timeout = Nothing }) "/t/places" (M.fromList [("q", "starbucks")])
  assertEqual "Valid read query" "ok" (status result))

facetsIntegrationTest token = TestCase (do
  let query = F.FacetsQuery { F.table        = Places
                            , F.search       = AndSearch ["Starbucks"]
                            , F.select       = ["country"]
                            , F.filters      = []
                            , F.geo          = Nothing
                            , F.limit        = Just 100
                            , F.minCount     = Just 1
                            , F.includeCount = False }
  result <- executeQuery (Options { token = token, timeout = Nothing }) query
  assertEqual "Valid facets query" "ok" (status result))

diffsIntegrationTest token = TestCase (do
  let query = D.DiffsQuery { D.table = Custom "t7RSEV", D.start = 1339123455775, D.end = 1339124455775 }
  result <- executeQuery (Options { token = token, timeout = Nothing }) query
  assertEqual "Valid diffs query" "ok" (status result))

geopulseIntegrationTest token = TestCase (do
  let query = G.GeopulseQuery { G.geo    = Point 34.06021 (-118.41828)
                              , G.select = [] }
  result <- executeQuery (Options { token = token, timeout = Nothing }) query
  assertEqual "Valid geopulse query" "ok" (status result))

geocodeIntegrationTest token = TestCase (do
  let query = GeocodeQuery $ Point 34.06021 (-118.41828)
  result <- executeQuery (Options { token = token, timeout = Nothing }) query
  assertEqual "Valid geopulse query" "ok" (status result))


multiIntegrationTest :: Token -> Test
multiIntegrationTest token = TestCase (do
  let query1 = ReadQuery { table = Places
                         , search = AndSearch ["McDonalds", "Burger King"]
                         , select = ["name"]
                         , limit = Just 50
                         , offset = Just 10
                         , includeCount = True
                         , geo = Just (Circle 34.06021 (-118.41828) 5000.0)
                         , sort = []
                         , filters = [EqualStr "name" "Stand"] }
  let query2 = query1 { filters = [EqualStr "name" "Xerox"] }
  results <- executeMultiQuery (Options { token = token, timeout = Nothing }) $ M.fromList [("query1", query1), ("query2", query2)]
  let result1 = results M.! "query1"
  let result2 = results M.! "query2"
  assertEqual "Valid multi query" ["ok","ok"] [status result1, status result2])

submitIntegrationTest :: Token -> Test
submitIntegrationTest token = TestCase (do
  let newValues = M.fromList [ ("name","Factual")
                             , ("address","1801 Avenue of the Stars, Suite 1450")
                             , ("country","USA")
                             , ("locality","Los Angeles") ]
  let write = S.Submit { S.table     = Custom "t7RSEV"
                       , S.user      = "drivertest"
                       , S.factualId = Nothing
                       , S.reference = Nothing
                       , S.comment   = Nothing
                       , S.values    = newValues }
  result <- executeWrite (Options { token = token, timeout = Nothing }) write
  assertEqual "Valid submit" "ok" (status result))

insertIntegrationTest :: Token -> Test
insertIntegrationTest token = TestCase (do
  let newValues = M.fromList [ ("name","Factual")
                             , ("address","1801 Avenue of the Stars, Suite 1450")
                             , ("country","USA")
                             , ("locality","Los Angeles") ]
  let write = I.Insert { I.table     = Custom "t7RSEV"
                       , I.user      = "drivertest"
                       , I.factualId = Nothing
                       , I.values    = newValues }
  result <- executeWrite (Options { token = token, timeout = Nothing }) write
  assertEqual "Valid insert" "ok" (status result))

flagIntegrationTest :: Token -> Test
flagIntegrationTest token = TestCase (do
  let write = L.Flag { L.table     = Custom "t7RSEV"
                     , L.user      = "drivertest"
                     , L.factualId = "97a38c06-cde1-402d-ad5a-4ae408530386"
                     , L.problem   = L.Inaccurate
                     , L.comment   = Nothing
                     , L.dataJSON  = Nothing
                     , L.fields    = Nothing
                     , L.reference = Nothing }
  result <- executeWrite (Options { token = token, timeout = Nothing }) write
  assertEqual "Valid flag" "ok" (status result))

clearIntegrationTest :: Token -> Test
clearIntegrationTest token = TestCase (do
  let write = C.Clear { C.table     = Custom "t7RSEV"
                      , C.user      = "drivertest"
                      , C.factualId = "97a38c06-cde1-402d-ad5a-4ae408530386"
                      , C.fields    = ["country"] }
  result <- executeWrite (Options { token = token, timeout = Nothing }) write
  assertEqual "Valid clear" "ok" (status result))

errorIntegrationTest :: Token -> Test
errorIntegrationTest token = TestCase (do
  result <- get (Options { token = token, timeout = Nothing }) "/t/foobarbaz" (M.empty)
  assertEqual "Invalud read query" "error" (status result))

blankReadQuery :: ReadQuery
blankReadQuery = ReadQuery { table = Places
                           , search = AndSearch []
                           , select = []
                           , limit = Nothing
                           , offset = Nothing
                           , filters = []
                           , geo = Nothing
                           , sort = []
                           , includeCount = False }

submitWrite :: S.Submit
submitWrite = S.Submit { S.table     = Places
                       , S.user      = "user123"
                       , S.factualId = Just "foobar"
                       , S.reference = Nothing
                       , S.comment   = Nothing
                       , S.values    = M.fromList [("key", "val")] }

insertWrite :: I.Insert
insertWrite = I.Insert { I.table     = Places
                       , I.user      = "user123"
                       , I.factualId = Just "foobar"
                       , I.values    = M.fromList [("key", "val")] }

flagWrite :: L.Flag
flagWrite = L.Flag { L.table       = Places
                   , L.factualId   = "foobar"
                   , L.problem     = L.Duplicate
                   , L.user        = "user123"
                   , L.comment     = Just "There was a problem"
                   , L.dataJSON    = Nothing
                   , L.fields      = Nothing
                   , L.reference   = Nothing }

clearWrite :: C.Clear
clearWrite = C.Clear { C.table       = Places
                     , C.factualId   = "foobar"
                     , C.user        = "user123"
                     , C.fields      = ["latitude", "longitude"] }
