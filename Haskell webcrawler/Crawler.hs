module Crawler where

import Network (withSocketsDo)
import Network.HTTP
import Network.URI (URI)
import System.IO
import qualified Control.Exception as X

import qualified Graph as G
import qualified Queue as Q
import qualified Parser as P
import qualified Searched as S
import qualified Statistics as St

{-  Representation Convention: String representing an adress to a node in a graph. The main environment for our program is the internet
                               so that would make it an URL.
    Representation Invariant: Has to be a valid adress to a node in the environment. Our main environment is the internet so it has
                               to be a valid URL in that context; "http://www.uu.se/" and not just "facebook.com" for example.
-}
type Url = String

{-  Representation Convention: HtmlFetcher represents a function that given a Url, 
                                returns the data from the corresponding node (website) in a graph (internet).
    Representation Invariant: True
-}
type HtmlFetcher = (Url -> IO String)

{- basicCrawlSum url depth getExternal
   Purpose:  To output the stets for a given site. 
   Pre:      See crawlGen...
   Variant:  See crawl...
   Post:     A list containing all the statistics gathered frmo the crawl. The statistical functions
             used in this case are those that St.statFnSamples1 create.
   Side fx:  See crawl...
   Examples: getLinkCountFromSite "http://www.lul.se/" 1 == [("Link count:",694)]
-}
basicCrawlSum :: Url -> Int -> Bool -> IO [(String, Int)]
basicCrawlSum url depth crawlExt = fmap (sumStats St.statFnSamples1) (fmap fst (basicCrawl url depth crawlExt))

{- sumStats functions listOfStatLists
   Purpose:  To combine statistics from a graph
   Pre:      Length of fns = length of amount of stats, fns are according to St.StatFn specification
   Post:     listOfStatLists as a single list with first element in each statlist combined with the first function in functions
             the second element in each statlist combined with the second function in functions etc.
   Examples: sumStats [(+), (-)] [(Node _ _ [1,2]), (Node _ _ [3,4]), (Node _ _ [5,6])] == [9, (-8)]
-}
sumStats :: [St.StatFn a] -> G.Graph URI P.Key a -> [a]
sumStats statFns graph = aux fns flat
  where
    flat = G.getStatistics graph
    fns  = map (\ (St.StatFn fs) -> snd fs) statFns
    
    aux :: [(a -> a -> a)] -> [[a]] -> [a]
    aux (f:fs) statsList@(s:ss) | length s == 1 = (foldl f (head s) (map head ss)):[]
                                | otherwise = foldl f (head s) (map head ss):(aux fs (map tail statsList))

{- basicCrawl url depth crawlExt
   Purpose:  To easily crawl the web using the input arguments and sample functions.
             All input arguments are forwarded to crawlGen.
   Pre:      See crawlGen...
   Post:     See crawlGen...
   Variant:  See crawl...
   Side fx:  See crawl...
   Examples: crawlWeb "http://www.uu.se/" 2 False == Returns a graph containing each internal subsite within 2 links of the called url as a node.
-}
basicCrawl :: Url -> Int -> Bool -> IO (G.Graph URI P.Key (String, Int), S.Searched)
basicCrawl url depth crawlExt = crawlGen url depth crawlExt getHtml St.statFnSamples1


{- crawlGen url depth htmlFetcher crawlExt statFns
   Purpose:  Initiates a crawl by supplying initial values and forwarding the input args.
   Pre:      See crawl...
   Post:     A tuple with a Graph and Searched list representing all pages that were crawled, both are according to their respective specification.
   Variant:  See crawl...
   Side fx:  See crawl...
   Examples: crawlGen "http://www.uu.se/" 1 getHtml False St.statFnSamples1
               == Returns tuple where first element is a graph containing each
                  nodes for each internal link within depth 1 of the url. The nodes'
                  stats are created through St.statFnSamples1. The second element is a S.Searched
                  structure containing every site that was searched.

                  TODO ^^^^^^ Belongs in post.
-}
crawlGen :: (Show statType) => Url -> Int -> Bool -> HtmlFetcher -> [St.StatFn statType] -> IO (G.Graph URI P.Key statType, S.Searched)
crawlGen url depth crawlExt htmlFetcher statFns = do
  (graphWrapper, searched) <- crawl q S.empty depth crawlExt htmlFetcher statFns
  return (fst graphWrapper, searched)
  where  
    q = Q.normalize $ Q.enqueue [P.urlToUri url] Q.empty -- normalize prevents depth reduction in first crawling round


{- crawl q searched depth crawlExt htmlFetcher statFns
   Purpose: To crawl a website. This is the main functions which makes all the other function calls and puts the data together.
            q:         Stores the links to goto next. It consists of two lists, one represents the current depth, 
                       the second represents the next depth.
            searched:  Stores all the nodes that we have already crawled. The purpose of this is to avoid looping.
            depth:     The depth variable which keeps track of what depth we are on. This counts down to zero from our maximum depth.
            crawlExt: Bool tells us if we want to crawl internally only, or include external links in the crawl.
            htmlFetcher: The function which gets the actual html. We have this to be able to test crawl in controlled environments(local data).
                         We have a getHtml function which returns html from the internet given a url.
            statFns: List of functions which are run on the data
   Pre: True
   Variant: depth (goes toward 0 as more levels of subpages are crawled)
   Post: A tuple. The first element is a Graph of nodes within 'depth' links away from url. Each node in the graph 
         consists of its URI, all found links on that site, all HTML of the  site in tagList-format, and a list of 
         data which is the results of all functions in statFns applied to the HTML. The second element in the tuple 
         is a Searched(See its specs) containing all the links we have crawled.
   Side-effects: Gets the data from an unknown source (htmlFetcher) and thus returns data from unknown source.
   Examples: crawl q S.empty 1 getHtml False [list of functions] == IO ( A graph , a Searched with all links we crawled )
             Examples are not very productive in this case since we are dealing with very messy input/outputs, see crawlerTests.
-}
crawl :: (Show statType) => Q.Queue URI -> S.Searched -> Int -> Bool -> HtmlFetcher -> [St.StatFn statType] -> IO (G.GraphWrapper URI P.Key statType, S.Searched)
crawl q searched depth crawlExt htmlFetcher statFns
  | Q.queueIsEmpty   q    = return ((G.emptyGraph, P.uriToKey), searched)
  | Q.dequeueIsEmpty q    = crawl (Q.normalize q) searched (depth-1) crawlExt htmlFetcher statFns -- lower depth when searching new lvl of links
  | S.member uri searched = crawl (Q.dequeue q)   searched' depth    crawlExt htmlFetcher statFns -- check if page already searched
  | otherwise = do
      putStrLn $ " uri: " ++ show uri
      html <- catchAny (htmlFetcher (P.uriToUrl uri)) (\e -> return "")
      let (node, q') = crawlAux uri html q (depth<1) crawlExt statFns
      (graphWrapper', searched'') <- crawl q' searched' depth crawlExt htmlFetcher statFns
      return (G.insert node graphWrapper', searched'')
    where
      uri = Q.getHead q
      searched' = (S.insertIncrement uri searched)
  

{- catchAny successIO exceptionIO
   Purpose:  Catches every exception thrown at it. We use this as a
             *compromise* to be able to keep crawling when unexpected things
             happen during HTML-fetching.
   Pre:      True
   Post:     If exception it returns exceptionIO, otherwise successIO.
   Side-fx:  TODO
   Examples: TODO
-}
catchAny :: IO a -> (X.SomeException -> IO a) -> IO a
catchAny sIO xIO = X.catch sIO xIO

    
{- crawlAux uri html queue crawlLinks crawlExt statFns
   Purpose:  Helper function to crawl. Gets the node of a html page along with the new queue.
   Pre:      True
   Post:     A tuple. First element: Node with elements (uri, all found links on site, HTML in tagTree-format, 
             and a list of data which is the results of all functions in statFns applied to the HTML. 
             Second element on the tuple is queue with found links added to it(external links also if crawlExt == true).
   Examples: crawlAux "http://www.uu.se/" "someHTML" q True False [list of functions] 
               == A tuple (node containing data of the site "http://www.uu.se/", a queue with new links added)
-}
crawlAux :: (Show statType) => URI -> String -> Q.Queue URI -> Bool -> Bool -> [St.StatFn statType] -> (G.Node URI statType, Q.Queue URI)
crawlAux uri html q lastCrawl crawlExt statFns = (node, q'')
  where
    q'         = Q.dequeue q
    tagList    = P.htmlToTagList html
    uriRegName = P.getRegName uri
    urls       = filter (\x -> x /= "") (P.getUrls tagList)
    uris       = P.urlsToUris (map (P.relativeToAbsolute uri) urls)

    urisToQueue= if crawlExt then uris else P.getInternalUris uris uriRegName
    -- list of statistic functions as applied to tagList
    stats      = map (\(St.StatFn fs) -> (fst fs) tagList) statFns
    
    node       = G.newNode uri uris tagList stats
    q''        = if lastCrawl then q' else Q.enqueue urisToQueue q'


{- getHtml url
   Purpose:  To fetch html given a url
   Pre:      Access to internet
   Post:     Html from the corresponding url
   Side effects: Output depends on external source (internet)
   Examples: getHTML "http://www.uu.se/" == HTML of http://www.uu.se/
-}
getHtml :: String -> IO String
getHtml url = withSocketsDo
    $ simpleHTTP (getRequest url) >>= getResponseBody

-- tests are in CrawlerTests.hs