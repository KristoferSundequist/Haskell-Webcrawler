{- About CrawlerTests.hs

   This module contains some functions which are IO-free (pure) versions of those in Crawler. 
   This makes solid testing possible using test data.

   All testcases (pure) from other modules are imported into this one,
   this way all tests of the program can be run centrally (using the function runAllTests or similar).
-}
module Tests where

import qualified Network.URI as URI
import Data.Maybe (fromJust)
import Test.HUnit
import qualified Data.Map as Map

import qualified Crawler as C
import qualified Graph as G
import qualified Queue as Q
import qualified Parser as P
import qualified Searched as S
import qualified Statistics as St
-- testdata

{- Representation Convention: A list of dummy webpages [(dURL, dHTML)]. dURL = the URL of a page, dHTML = HTML of that page.
   Representation Invariant:  All links in dHTML must point toward an URL that exists in some dURL in the list.
                              dURL must be unique.
   Examples: (numbers represent urls/links)
     Valid:   [("1", "2,3"), ("2", "1,3"), ("3", "3"), ("4", "")]
     Invalid: [("1", "3"),   ("2", "")]
-}
type TestData = [(String, String)]

testData1 :: TestData
testData2 :: TestData

{- testData1
   Crawling http://www.foo.com/1/ with depth 7 should traverse the entire graph assuming we start on depth 0.
   This means (A) the length of the node list should be 13 and (B) link 9 should have been seen twice

   Same graph in simplified form:
     [(1,  [2,3]),
      (2,  [4]),
      (3,  [10]),
      (4,  [5]),
      (5,  [6]),
      (6,  [7]),
      (7,  [8]),
      (8,  [9]),
      (9,  [12]),
      (10, [11]),
      (11, [8]),
      (12, [13]),
      (13, [])] -}
testData1 =
  [("http://www.foo.com/1/",  "<body><a href=\"http://www.foo.com/2/\"></a><a href=\"http://www.foo.com/3/\"></a></body>"),
   ("http://www.foo.com/2/",  "<body><a href=\"http://www.foo.com/4/\"></a></body>"),
   ("http://www.foo.com/3/",  "<body><a href=\"http://www.foo.com/10/\"></a></body>"),
   ("http://www.foo.com/4/",  "<body><a href=\"http://www.foo.com/5/\"></a></body>"),
   ("http://www.foo.com/5/",  "<body><a href=\"http://www.foo.com/6/\"></a></body>"),
   ("http://www.foo.com/6/",  "<body><a href=\"http://www.foo.com/7/\"></a></body>"),
   ("http://www.foo.com/7/",  "<body><a href=\"http://www.foo.com/8/\"></a></body>"),
   ("http://www.foo.com/8/",  "<body><a href=\"http://www.foo.com/9/\"></a></body>"),
   ("http://www.foo.com/9/",  "<body><a href=\"http://www.foo.com/12/\"></a></body>"),
   ("http://www.foo.com/10/", "<body><a href=\"http://www.foo.com/11/\"></a></body>"),
   ("http://www.foo.com/11/", "<body><a href=\"http://www.foo.com/8/\"></a></body>"),
   ("http://www.foo.com/12/", "<body><a href=\"http://www.foo.com/13/\"></a></body>"),
   ("http://www.foo.com/13/", "<body></body>")]

{- testData2: Infinitely large graph

   Start on depth 0, crawling over http://www.foo.com/1/ internally with depth d => a node list with length u^(d+1)-1, u = number of unique internal links per page.  That list should include all pages up until u^(d+1)-1.
   
   When crawling over external pages too every page has 1 external link. There are duplicate links on every dupPeriod page, these should naturally not be clicked.
   
   Warning, infinite loop if the url searched for doesn't exist in here.  Only http://www.foo.com/n/, n integer >= 1 and http://www.ext.com/m, m 1 through 5 exist!

   Example: Crawling over 1 with depth 2, u = 2 will return a node list with length 2^(2+1)-1 = 7 -}
testData2 = extPages ++ listComp
          where
            extCount = testData2ExtCount    -- total external pages in testData2
            dupPeriod= testData2DupPeriod   -- the periodicy with which duplicate links appear
            u        = 2 -- unique links per page
            extPages = map (\x -> ("http://www.ext.com/" ++ show x ++ "/", "")) [1..extCount]
            listComp = [(url, htmlPre ++ aTags ++ aTagDup ++ aTagExt ++ htmlPost) |
              a <- [1..],
              let htmlPre = "<body>", let htmlPost = "</body>",
              let b       = 2 - u + u^4 + u * (a - u^3),
              let url     = "http://www.foo.com/" ++ show a ++ "/",
              let aTagPre = "<a href=\"http://www.foo.com/",
              let aTags   = foldl (\ acc x -> acc ++ aTagPre ++ show x ++ "/\"></a>") "" [b..b+u-1],
              let aTagDup = if (a `mod` dupPeriod) == 0 then aTagPre ++ show (a+1) ++ "/\"></a>" else "",
              let aTagExt = "<a href=\"http://www.ext.com/" ++ show (1 + (a-1) `mod` extCount) ++ "/\"></a>"]

testData2ExtCount    = 5 -- total external pages in testData2
testData2DupPeriod   = 4 -- the periodicy with which duplicate links appear

-- testData3: another dummy webgraph
testData3 =
  [("http://www.foo.com/1/",  "<body><a href=\"http://www.foo.com/2/\"></a><a href=\"http://www.foo.com/3/\"></a>" ++ contents1 ++ "</body>"),
   ("http://www.foo.com/2/",  "<body><a href=\"http://www.foo.com/4/\"></a>" ++ contents2 ++ "</body>"),
   ("http://www.foo.com/3/",  "<body><a href=\"http://www.foo.com/5/\"></a>" ++ contents3 ++ "</body>"),
   ("http://www.foo.com/4/",  "<body><a href=\"http://www.foo.com/5/\"></a>" ++ contents4 ++ "</body>"),
   ("http://www.foo.com/5/",  "<body>" ++ contents5 ++ "</body>")]
  where
    contents1 = "<div>This is some sample text</div>"
    contents2 = "<div>Five oranges are more than six apples</div>"
    contents3 = "<div>An elephant eats six oranges per day</div>"
    contents4 = "<div>Oranges program text</div>"
    contents5 = "<div>These are good apples. Just kidding, no such thing.</div>"
    

-- test functions

{- Representation Convention: Same as Crawler.HtmlFetcher but returns "a" instead of "IO a".
   Representation Invariant:  See Crawler.HtmlFetcher...
-}
type PureHtmlFetcher = (String -> String)

{- crawlPure q searched depth pureHtmlFetcher crawlExt statFns
   Purpose:  To crawl a dummy website using a pure function. Same as crawl except it
               takes a PureHtmlFetcher instead of HtmlFetcher,
               doesn't have side effects and returns "a" instead of "IO a".
   Pre:      See Crawler.crawl...
   Variant:  See Crawler.crawl...
   Post:     Same as crawl but returns "a" instead of "IO a".
   Examples: crawlPure (Q.enqueue [P.urlToUri "http://www.foo.com/1/"] Q.empty) 1 (getTestHtml testData1) False St.statFnSamples1
              returns a tuple (GraphWrapper, S.Searched) where both elements are according to the respective specification of their types
-}
crawlPure :: (Show statType) => Q.Queue URI.URI -> S.Searched -> Int -> Bool -> PureHtmlFetcher -> [St.StatFn statType] -> (G.GraphWrapper URI.URI P.Key statType, S.Searched)
crawlPure q searched depth crawlExt htmlFetcher statFns
  | Q.queueIsEmpty   q    = ((G.emptyGraph, P.uriToKey), searched)
  | Q.dequeueIsEmpty q    = crawlPure (Q.normalize q) searched (depth-1) crawlExt htmlFetcher statFns -- lower depth when searching new lvl of links
  | S.member uri searched = crawlPure (Q.dequeue q)   searched' depth    crawlExt htmlFetcher statFns -- check if page already searched
  | otherwise = (G.insert node graphWrapper', searched'')
  where
      html = htmlFetcher $ P.uriToUrl uri
      uri = Q.getHead q
      searched' = (S.insertIncrement uri searched)
      (graphWrapper', searched'') = crawlPure q' searched' depth crawlExt htmlFetcher statFns
      (node, q') = C.crawlAux uri html q (depth<1) crawlExt statFns

{- crawlGenPure url depth crawlExt pureHtmlFetcher statFns
   Purpose:  Initiates a crawl with crawlPure. This function basically forwards input args and initial values to crawlPure.
   Pre:      See Crawler.crawl...
   Variant:  See Crawler.crawl...
   Post:     A tuple with a Graph and Searched list representing all pages that were crawled, both are according to their respective specification.

   Examples: crawlGenPure "http://www.foo.com/1/" 1 False (getTestHtml testData1) St.statFnSamples1 == 
              returns a tuple (G.Graph, S.Searched), where both elements are according to specification of their respective type.

-}
crawlGenPure :: (Show statType) => C.Url -> Int -> Bool -> PureHtmlFetcher -> [St.StatFn statType] -> (G.Graph URI.URI P.Key statType, S.Searched)
crawlGenPure url depth crawlExt htmlFetcher statFns = (fst graphWrapper, searched)
  where
    (graphWrapper, searched) = crawlPure q S.empty depth crawlExt htmlFetcher statFns
    q = Q.normalize $ Q.enqueue [P.urlToUri url] Q.empty -- normalize prevents depth reduction in first crawling round

{- getTestHtml testData url
   Purpose:  To fetch the html of testData.
   Pre:      url must exist in testData (fst of some tuple in testdata == url)
   Post:     A string representing the html of the url.
             If the url doesn't exist in testData an empty string is returned.
   Examples: getTestHtml [("1", ""), ("2", "1")]             "2" == "1"
             getTestHtml [(show n, show (n^2)) | n <- [1..]] "2" == "4"
             getTestHtml [("1", "2"), ("2", "")]             "3"    Pre not satisfied! Empty list.
             getTestHtml [ (show n, "") | n <- [3..] ]       "2"    Pre not satisfied! Infinite loop.
-}
getTestHtml :: TestData -> String -> String
getTestHtml testData url = snd $ head (filter (\(a, b) -> a == url) testData)

-- tests

testCases :: [Test]
testCases = [test1, test2, test3, test4, test5]

-- test cases for all modules
allTestCases = testCases ++ G.testCases ++ Q.testCases ++ P.testCases ++ S.testCases

-- getTestHtml
test1 = TestCase $ assertBool ("CrawlerTests: getTestHtml " ++ show [("1", ""), ("2", "1")] ++ " " ++ show "2") $
  getTestHtml [("1", ""), ("2", "1")] "2" == "1"
test2 = TestCase $ assertBool ("CrawlerTests: getTestHtml [(show n, show (n^2)) | n <- [1..]] "    ++ show "2") $
  getTestHtml [(show n, show (n^2)) | n <- [1..]] "2" == "4"

-- crawlGenPure with testData1
test3 = TestCase $ assertBool ("CrawlerTests: crawlGenPure with testData1, correct depth ") $ (length1 == 13 && length2 == 12)
  where
    length1 = length $ Map.toList $ snd $ crawlGenPure "http://www.foo.com/1/" 7 False (getTestHtml testData1) St.statFnSamples1
    length2 = length $ Map.toList $ snd $ crawlGenPure "http://www.foo.com/1/" 6 False (getTestHtml testData1) St.statFnSamples1
test4 = TestCase $ assertBool ("CrawlerTests: crawlGenPure with testData1, count ") $ (countOf8 == 2)
  where
    (Just countOf8) = S.getCount (fromJust (URI.parseURI "http://www.foo.com/8/")) $ snd $
                      crawlGenPure "http://www.foo.com/1/" 7 True (getTestHtml testData1) St.statFnSamples1 
-- crawlGenPure with testData2
test5 = TestCase $ assertBool ("CrawlerTests: crawlGenPure with testData2, correct length ") $ (foldr (&&) True conditions)
  where
    conditions = [ listLength == expected | depth <- [0..8]
                   , let listLength = length $ Map.toList $ snd $ crawlGenPure "http://www.foo.com/1/"
                                      depth False (getTestHtml testData2) St.statFnSamples1
                   , let expected   = 2^(depth + 1) - 1 -- according to testData2 comments
                 ]

-- for running all the tests
runTests = runTestTT $ TestList testCases

-- for running all the tests in all modules
runAllTests = runTestTT $ TestList allTestCases

-- OLD TESTS (Remove later)

ioTest1   = C.basicCrawl "http://www.uu.se/"     0 False
oTest2 d  = crawlGenPure "http://www.foo.com/1/" d False (getTestHtml testData2) St.statFnSamples1
ioTest3 d = C.basicCrawl "http://www.uu.se/"     d False