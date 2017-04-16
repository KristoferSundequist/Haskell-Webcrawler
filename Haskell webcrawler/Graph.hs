module Graph where

import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Test.HUnit hiding (Node)

import qualified Parser as P

{- Representation Convention: A Node containing a label and its associated data.
   Representation Invariant: True
   
   Possible TODO: It's not fully generic,
     tagList can be a third argument instead of being of typeP.TagList...
-}
data Node label statType = Node {
    label      :: label      -- Label for the node
  , adjacent   :: [label]    -- Adjacency list consisting of labels of neighbours
  , tagList    :: P.TagList
  , statistics :: [statType]
  } deriving (Eq, Show)


{- Representation Convention: A graph of nodes represented by a Map from Data.Map.
                              label is the type of label used in Node.
                              key is the type of key used by the Map.
                              statType is the type of stat used in nodes.
   Representation Invariant:  True
-}
type Graph label key statType = Map.Map key (Node label statType)


{- Representation Convention: A wrapper around a Graph. It's second element is a function
                              which converts labels to keys, it'll be used by functions such as insert.
   Representation Invariant:  The function labelToKey must return unique keys for every unique label.
                              (If it's not totally unique the user must understand that they will lose information
                               and decide whether that loss is acceptable.)

   Examples: Using (`mod` 3) as label to key function will make every third keys (from Int) clash!
             This is unacceptable in many cases.
-}
type GraphWrapper label key statType = (Graph label key statType, (label -> key))

{- newNode label adjacencyList tagList statistics
   Purpose:   Create a new node with given arguments.
   Pre:       True
   Post:      A new node with the given arguments as respective values.
   Examples:  newNode "lbl1" ["lbl2","lbl3"] [] ["Adj: 2"] == 
                Node { label = "lbl1", adjacent = ["lbl2","lbl3"], tagList = [], statistics = ["Adj: 2"]} 
-}
newNode :: label -> [label] -> P.TagList -> [statType] -> Node label statType
newNode lbl adj tagString stats = Node lbl adj tagString stats

{- emptyGraph
   Purpose:  Returns an empty graph
   Pre:      True
   Post:     A graph without any nodes (empty).
   Examples: (emptyGraph :: Graph Int Int Int) == Map.fromList []
-}
emptyGraph :: Graph label key statType
emptyGraph = Map.empty

{- emptyGraphWrapper labelToKey
   Purpose:  Returns an empty GraphWrapper.
   Pre:      True (see GraphWrapper)
   Post:     A GraphWrapper with empty Graph and the function labelToKeyFunction.
   Examples: (emptyGraphWrapper (*2)) produces an empty GraphWrapper with the function (*2).
-}
emptyGraphWrapper :: (label -> key) -> GraphWrapper label key statType
emptyGraphWrapper labelToKeyFn = (emptyGraph, labelToKeyFn)

{- insert node graphWrapper
   Purpose:  Inserts node into the Graph of graphWrapper.
   Pre:      True
   Post:     A new graphWrapper with node inserted into its Graph.
   Examples: fst ( insert (Node { label = 1, adjacent = [], tagList = [], statistics = []})
                          (Map.fromList [], (*2)) )
               == Map.fromList [ (2, Node { label = 1, adjacent = [], tagList = [], statistics = []}) ]

    Pre not satisfied for GraphWrapper! `mod` 3 means many keys will clash.
             fst ( insert (Node { label = 5, adjacent = [3], tagList = [], statistics = []})
                   ( insert (Node { label = 2, adjacent = [0], tagList = [], statistics = []})
                     (Map.fromList [], (`mod` 3))
                   )
                 )
               == Map.fromList [(2,Node {label = 5, adjs = [3], tagList = [], statistics = []})]
-}
insert :: (Ord key) => Node label statType -> GraphWrapper label key statType -> GraphWrapper label key statType
insert node (map, labelToKey) = (Map.insert (labelToKey (label node)) node map, labelToKey)


{- member label graphWrapper
   Purpose:  Checks if there exists a node associated with label in the Graph of graphWrapper.
   Pre:      True
   Post:     True if a node with label exists in the Graph in graphWrapper, False otherwise.
   Examples: member 1 ( Map.fromList [ (2, Node { label = 1, adjacent = [], tagList = [], statistics = []}) ],
                        (*2)
                      ) == True

             member 2 ( Map.fromList [ (2, Node { label = 1, adjacent = [], tagList = [], statistics = []}) ],
                                     (*2)
                      ) == False
-}
member :: (Ord key) => label -> GraphWrapper label key statType -> Bool
member lbl (map, lblToKey) = Map.member (lblToKey lbl) map


{- getNode label graphWrapper
   Purpose:  Gets the node associated with label in Graph of graphWrapper if it exists.
   Pre:      True
   Post:     Returns Just node if the node existed and Nothing otherwise.
   Examples: getNode 1 (Map.fromList [ (2, Node { label = 1, adjacent = [], tagList = [], statistics = []}) ],
                        (*2)
                       ) == Just (Node {label = 1, adjacent = [], tagList = [], statistics = []})

             getNode 2 (Map.fromList [ (2, Node { label = 1, adjacent = [], tagList = [], statistics = []}) ],
                        (*2)
                       ) == Nothing
-}
getNode :: (Ord key) => label -> GraphWrapper label key statType -> Maybe (Node label statType)
getNode lbl (graph, lblToKey) = Map.lookup (lblToKey lbl) graph


{- getNodes graph
   Purpose:  Gets all nodes in a graph.
   Pre:      True.
   Post:     A list of nodes.
   Examples: getNodes (Map.fromList []) == []
             getNodes (Map.fromList [(1, Node { label = 1, adjacent = [], tagList = [], statistics = []}),
                                     (2, Node { label = 2, adjacent = [], tagList = [], statistics = []})])
                                     == [Node { label = 1, adjacent = [], tagList = [], statistics = []},
                                         Node { label = 2, adjacent = [], tagList = [], statistics = []}]
-} 
getNodes :: Graph label key statType -> [Node label statType]
getNodes map = Map.fold (:) [] map

{- getNodes graph
   Purpose:  Gets all statistics from a graph.
   Pre:      True.
   Post:     A list of stats, each element being the stats of a node in graph.
   Examples: getStatistics (Map.fromList []) == []
             getStatistics (Map.fromList [(7, Node { label = 7, adjacent = [3,4], tagList = [], statistics = ["AdjCount: 2", "FirstAdj: 3"]}),
                                          (8, Node { label = 8, adjacent = [5], tagList = [], statistics = ["AdjCount: 1", "FirstAdj: 5"]})])
                                     == [["AdjCount: 2","FirstAdj: 3"],["AdjCount: 1","FirstAdj: 5"]]
-}
getStatistics :: Graph label key statType -> [[statType]]
getStatistics graph = map statistics (getNodes graph)

--prettyPrintWordCounts :: (Show label) => Graph label key (String, Int) -> String
prettyPrintWordCounts graph = labeledCount --Prelude.map ((\x -> read x :: [(String,Int)]) . fst . head) (getStatistics graph)
  where
    labeledCount = foldl (++) "Word counts for the crawl: " (map prettifyNode (getNodes graph))
    prettifyNode n = foldl (++) ("\n\n" ++ (show (label n)) ++ " \n") $
                     prettifyCount $ fst $ head $ statistics n
    prettifyCount xs = map (\ (word, count) -> word ++ " " ++ (show count) ++ ", ") (read xs :: [(String,Int)])

-- tests

testCases :: [Test]
testCases = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12]

-- newNode
test1 = TestCase $ assertBool ("Graph: newNode") $
          newNode "lbl1" ["lbl2","lbl3"] [] ["Adj: 2"]
                       == Node { label = "lbl1"
                               , adjacent = ["lbl2","lbl3"]
                               , tagList = []
                               , statistics = ["Adj: 2"]} 

-- emptyGraph
test2 = TestCase $ assertBool ("Graph: emptyGraph") $ (emptyGraph :: Graph Int Int Int) == Map.fromList []

-- insert
test3 = TestCase $ assertBool ("Graph: insert") $ lh == rh
  where
    lh :: Graph Int Int Int
    lh = fst $ insert (Node { label = 1, adjacent = [], tagList = [], statistics = []})
                      (Map.fromList [], (*2))

    rh :: Graph Int Int Int
    rh = Map.fromList [ (2, Node { label = 1, adjacent = [], tagList = [], statistics = []}) ]

test4 = TestCase $ assertBool ("Graph: insert bad labelToKey") $ lh == rh
  where
    lh :: Graph Int Int Int
    lh =  fst $ insert (Node { label = 5, adjacent = [3], tagList = [], statistics = []})
              $ insert (Node { label = 2, adjacent = [0], tagList = [], statistics = []})
                       (Map.fromList [], (`mod` 3))

    rh :: Graph Int Int Int
    rh = Map.fromList [(2,Node {label = 5, adjacent = [3], tagList = [], statistics = []})]

-- member
test5 = TestCase $ assertBool ("Graph: member") $ expr == True
  where
    expr = member 1 ( Map.fromList [ (2, Node { label = 1, adjacent = [], tagList = [], statistics = []}) ],
                      (*2) )

test6 = TestCase $ assertBool ("Graph: member non-existent-node") $ expr == False
  where
    expr = member 2 ( Map.fromList [ (2, Node { label = 1, adjacent = [], tagList = [], statistics = []}) ],
                      (*2) )

-- getNode
test7 = TestCase $ assertBool ("Graph: getNode") $ lh == rh
  where
    lh :: Maybe (Node Int String)
    lh = getNode 1 ( Map.fromList [ (2, Node { label = 1, adjacent = [], tagList = [], statistics = []}) ],
                     (*2) )
    
    rh :: Maybe (Node Int String)
    rh = Just (Node {label = 1, adjacent = [], tagList = [], statistics = []})

test8 = TestCase $ assertBool ("Graph: getNode non-existent-node") $ lh == Nothing
  where
    lh :: Maybe (Node Int String)
    lh = getNode 2 (Map.fromList [ (2, Node { label = 1, adjacent = [], tagList = [], statistics = []}) ],
                    (*2))

-- getGraphNodes
test9  = TestCase $ assertBool ("Graph: getNodes emptyGraph") $
           getNodes (Map.fromList [] :: Graph Int Int Int) == []
test10 = TestCase $ assertBool ("Graph: getNodes") $
          getNodes (Map.fromList [(1, Node { label = 1, adjacent = [], tagList = [], statistics = [] :: [Int]}),
                                  (2, Node { label = 2, adjacent = [], tagList = [], statistics = []})])
                                  == [Node { label = 1, adjacent = [], tagList = [], statistics = [] :: [Int]},
                                      Node { label = 2, adjacent = [], tagList = [], statistics = []}]

-- getGraphStatistics
test11 = TestCase $ assertBool ("Graph: getStatistics emptyGraph") $ getStatistics (Map.fromList [] :: Graph Int Int Int) == []
test12 = TestCase $ assertBool ("Graph: getStatistics") $
           getStatistics (Map.fromList [(7, Node { label = 7, adjacent = [3,4], tagList = [], statistics = ["AdjCount: 2", "FirstAdj: 3"]}),
                                        (8, Node { label = 8, adjacent = [5],   tagList = [], statistics = ["AdjCount: 1", "FirstAdj: 5"]})])
                                   == [["AdjCount: 2","FirstAdj: 3"],["AdjCount: 1","FirstAdj: 5"]]

-- for running all the tests
runTests = runTestTT $ TestList testCases
