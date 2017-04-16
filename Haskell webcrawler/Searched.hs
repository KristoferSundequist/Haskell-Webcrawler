module Searched (insert, insertIncrement, insertMany,
                 empty, Searched, member, getCount, testCases) where

import qualified Data.Map as Map
import Data.Map (empty)
import Data.Maybe (fromJust)
import qualified Network.URI as URI
import Test.HUnit

import qualified Parser as P

{- Representation Convention: An association list that associates a webpage to the amount of times it has been spotted.
                              The int is the amount of times the webpage has been spotted.
   Representation Invariant:  True
-}
type Searched = Map.Map P.Key Int

{- insertIncrement uri searched
   Purpose:  Inserts a node with key (P.uriToKey uri) to searched and 1 as node value, if the key already exists in searched: adds one to the value.
   Pre:      True
   Post:     Searched with an additional node with key (P.uriToKey uri) inserted to it and the corresponding int added with one.
             If the key already exists: searched with value of key (P.uriToKey uri) incremented by 1.
   Examples  insertIncrement (fromJust $ URI.parseURI "http://www.uu.se/") empty == Map.fromList [("www.uu.se/",1)]
             insertIncrement (fromJust $ URI.parseURI "http://www.uu.se/") (Map.fromList [("www.uu.se/",1)]) == Map.fromList [("www.uu.se/",2)]
-}
insertIncrement :: URI.URI -> Searched -> Searched
insertIncrement uri map = Map.insertWith (+) (P.uriToKey uri) 1 map

{- member uri searched
   Purpose:  Check if uri is a member of searched
   Pre:      True
   Post:     True if uri is a member of searched, else false.
   Examples: member (fromJust $ URI.parseURI "http://www.uu.se/") (Map.fromList [("www.uu.se/" ,1)]) == True  
             member (fromJust $ URI.parseURI "http://www.notuu.se/") (Map.fromList [("www.uu.se/" ,1)]) == False
-}
member :: URI.URI -> Searched -> Bool
member uri searched = Map.member (P.uriToKey uri) searched


{- insertMany uris map
   Purpose:  To insert a list of URIS into a map
   Pre:      True
   Post:     map with all uris inserted
   Examples: insertMany [fromJust $ URI.parseURI "http://www.uu.se/", fromJust $ URI.parseURI "http://www.uu.se/subsida"] (Map.fromList [("www.a.se/" ,1)])
              == Map.fromList [("www.a.se/",1),("www.uu.se/",1),("www.uu.se/subsida",1)]
-}
insertMany :: [URI.URI] -> Searched -> Searched
insertMany uris map = foldr insertIncrement map uris

{- getCount uri map
   Purpose:  To get the count of the corresponding uri in map
   Pre:      True
   Post:     Just count if uri is a member of map, else Nothing
   Examples: getCount (fromJust $ URI.parseURI "http://www.a.se/")
              (insertMany [fromJust $ URI.parseURI "http://www.uu.se/", fromJust $ URI.parseURI "http://www.uu.se/subsida"]
                          (Map.fromList [("www.a.se/",1)])
              ) == Just 1
-}
getCount :: URI.URI -> Searched -> Maybe Int
getCount uri map = Map.lookup (P.uriToKey uri) map

{- insert uri map
   Purpose:  To insert a uri into a map
   Pre:      True
   Post:     if uri already exists in map then map, else map with uri inserted
   Examples: insert (fromJust $ URI.parseURI "http://www.uu.se/") empty
               == Map.fromList [("www.uu.se/",1)]
			       insert (fromJust $ URI.parseURI "http://www.uu.se/") (Map.fromList [("www.uu.se/", 1)])
               == Map.fromList [("www.uu.se/",1)]
-}
insert :: URI.URI -> Searched -> Searched
insert uri map = Map.insert (P.uriToKey uri) 1 map

--tests

testCases :: [Test]
testCases = [test1,test2,test3,test4,test5,test6,test7,test8,test9]

-- insertIncrement
test1 = TestCase $ assertBool ("Searched: insertIncrement into empty") $
          insertIncrement (fromJust $ URI.parseURI "http://www.uu.se/") empty == Map.fromList [("www.uu.se/",1)]

test2 = TestCase $ assertBool ("Searched: insertIncrement") $ insertIncrement (fromJust $ URI.parseURI "http://www.uu.se/")
          (Map.fromList [("www.uu.se/",1)]) == Map.fromList [("www.uu.se/",2)]

-- member
test3 = TestCase $ assertBool ("Searched: member") $
          member (fromJust $ URI.parseURI "http://www.uu.se/")    (Map.fromList [("www.uu.se/", 1)]) == True
test4 = TestCase $ assertBool ("Searched: member2") $
          member (fromJust $ URI.parseURI "http://www.notuu.se/") (Map.fromList [("www.uu.se/", 1)]) == False

-- insert
test5 = TestCase $ assertBool ("Searched: insert into empty") $
            insert (fromJust $ URI.parseURI "http://www.uu.se/") empty
          == Map.fromList [("www.uu.se/", 1)]
test6 = TestCase $ assertBool ("Searched: insert into existing") $
            insert (fromJust $ URI.parseURI "http://www.uu.se/") (Map.fromList [("www.uu.se/", 1)])
          == Map.fromList [("www.uu.se/", 1)]

-- insertMany
test7 = TestCase $ assertBool ("Searched: insertMany") $
            insertMany [fromJust $ URI.parseURI "http://www.uu.se/", fromJust $ URI.parseURI "http://www.uu.se/subsida"]
              (Map.fromList [("www.a.se/" ,1)])
          == Map.fromList [("www.a.se/",1),("www.uu.se/",1),("www.uu.se/subsida",1)]

-- getCount
test8 = TestCase $ assertBool ("Searched: getCount") $
            insert (fromJust $ URI.parseURI "http://www.uu.se/") empty
          == Map.fromList [("www.uu.se/",1)]
test9 = TestCase $ assertBool ("Searched: getCount2") $
            insert (fromJust $ URI.parseURI "http://www.uu.se/") (Map.fromList [("www.uu.se/", 1)])
          == Map.fromList [("www.uu.se/",1)]

-- for running all the tests
runTests = runTestTT $ TestList testCases
