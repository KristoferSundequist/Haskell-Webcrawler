module Queue (Queue, dequeue, enqueue, getHead, empty, normalize,
              queueIsEmpty, enqueueIsEmpty, dequeueIsEmpty, testCases) where

import Test.HUnit

{- Representation convention: Q (enqueue, dequeue)
     enqueue is a list of items to be added to dequeue.
     dequeue is a list of items that are next in line.
     The queue is empty if enqueue and dequeue are empty.
   Representation invariant: True.
-}
data Queue a = Q ([a], [a])
  deriving (Eq, Show)

{- empty
   Purpose:  Returns an empty queue.
   Pre:      True
   Post:     A queue with an empty enqueue and dequeue list.
   Examples: empty == Q ([], [])
-}
empty :: Queue a
empty = Q ([], [])

{- dequeue queue
   Purpose:  Removes the head of the queue and returns the new queue.
   Pre:      The queue isn't empty.
   Post:     queue but with the head removed.
   Variant:  length of dequeue list (>0 after normalize)
   Examples: dequeue (Q ([1], [2,3])) == Q ([1],[3])
             dequeue (Q ([3,4], []))  == dequeue (Q ([], [4,3]))
             dequeue (Q ([3,4], []))  == Q ([],[3])
-}
dequeue :: Queue a -> Queue a
dequeue q@(Q (enq, [])) = dequeue (normalize q) 
dequeue (Q (enq, d:ds)) = Q (enq, ds)

{- getHead queue
   Purpose:  Returns the head of the dequeue list.
   Pre:      Dequeue list isn't empty.
   Post:     The head of the dequeue list.
   Examples: getHead (Q ([],[1,2])) == 1
             getHead (Q ([2,1],[])) == Exception! (Precondition not satisfied)
-}
getHead :: Queue a -> a
getHead (Q (_, x:_)) = x

{- enqueue items queue
   Purpose:  Add items to the queue.
   Pre:      True.
   Post:     New queue with items added in reverse to beginning of its enqueue list.
   Variant:  Length of xs.
   Examples: enqueue [2,1] (Q ([3], [])) == (Q ([1,2,3], []))
-}
enqueue :: [a] -> Queue a -> Queue a
enqueue xs (Q (enq, deq)) = Q (enq', deq)
  where
    enq' = foldl (\acc x -> x:acc) enq xs

{- normalize queue
   Purpose:  If dequeue is empty it replaces it with the reversal of enqueue
             and empties enqueue, otherwise queue is returned.
   Pre:      True.
   Post:     If dequeue is empty a new queue where dequeue is queue's enqueue
             in reverse and enqueue is empty.
             If dequeue isn't empty queue is returned.
   Examples: normalize (Q ([3,2], [1])) == (Q ([3,2], [1])) 
             normalize (Q ([3,2], []))  == (Q ([], [2,3]))
-}
normalize :: Queue a -> Queue a
normalize (Q (enq, [])) = Q ([], reverse enq)
normalize q             = q

{- queueIsEmpty queue
   Purpose:  To check if the enqueue and dequeue list of the is queue empty.
   Pre:      True.
   Post:     True if both the enqueue and dequeue list is empty, False otherwise.
   Examples: queueIsEmpty (Q ([],[]))  == True
             queueIsEmpty (Q ([1],[])) == False
-}
queueIsEmpty :: Queue a -> Bool
queueIsEmpty (Q ([], [])) = True
queueIsEmpty _            = False

{- dequeueIsEmpty queue
   Purpose:  To check if the dequeue list of the queue is empty.
   Pre:      True.
   Post:     True if the dequeue list is empty, False otherwise.
   Examples: dequeueIsEmpty (Q ([1],[])) == True
             dequeueIsEmpty (Q ([],[1])) == False
-}
dequeueIsEmpty :: Queue a -> Bool
dequeueIsEmpty (Q (_, [])) = True
dequeueIsEmpty _           = False 

{- enqueueIsEmpty queue
   Purpose:  To check if the enqueue list of the queue is empty.
   Pre:      True.
   Post:     True if the enqueue list is empty, False otherwise.
   Examples: enqueueIsEmpty (Q ([1],[])) == False
             enqueueIsEmpty (Q ([],[1])) == True
-}
enqueueIsEmpty :: Queue a -> Bool
enqueueIsEmpty (Q ([], _)) = True
enqueueIsEmpty _           = False

-- tests

testCases :: [Test]
testCases = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12]

-- empty
test1 = TestCase $ assertBool ("Queue: empty") $ empty == (Q ([], []::[Int]))

-- dequeue
test2 = TestCase $ assertBool ("Queue: dequeue $ " ++ show (Q ([1],[2,3])) ) $ dequeue (Q ([1],[2,3])) == Q ([1],[3])
test3 = TestCase $ assertBool ("Queue: dequeue $ " ++ show (Q ([3,4],[]))  ) $ dequeue (Q ([3,4],[]))  == dequeue (Q ([],[4,3]))

-- getHead
test4 = TestCase $ assertBool ("Queue: getHead $ " ++ show (Q ([],[1,2])) ) $ getHead (Q ([],[1,2])) == 1

-- normalize
test5 = TestCase $ assertBool ("Queue: normalize $ " ++ show (Q ([3,2],[1])) ) $ normalize (Q ([3,2],[1])) == (Q ([3,2],[1])) 
test6 = TestCase $ assertBool ("Queue: normalize $ " ++ show (Q ([3,2],[]))  ) $ normalize (Q ([3,2],[]))  == (Q ([],[2,3]))

-- queueIsEmpty
test7 = TestCase $ assertBool ("Queue: queueIsEmpty $ " ++ show (Q ([],[] :: [Int])) ) $ queueIsEmpty (Q ([],[]))  == True
test8 = TestCase $ assertBool ("Queue: queueIsEmpty $ " ++ show (Q ([1],[]))         ) $ queueIsEmpty (Q ([1],[])) == False

-- dequeueIsEmpty
test9  = TestCase $ assertBool ("Queue: dequeueIsEmpty $ " ++ show (Q ([1],[])) ) $ dequeueIsEmpty (Q ([1],[])) == True
test10 = TestCase $ assertBool ("Queue: dequeueIsEmpty $ " ++ show (Q ([],[1])) ) $ dequeueIsEmpty (Q ([],[1])) == False

-- enqueueIsEmpty
test11 = TestCase $ assertBool ("Queue: enqueueIsEmpty" ++ show (Q ([1],[])) ) $ enqueueIsEmpty (Q ([1],[])) == False
test12 = TestCase $ assertBool ("Queue: enqueueIsEmpty" ++ show (Q ([],[1])) ) $ enqueueIsEmpty (Q ([],[1])) == True

-- for running all the tests
runTests = runTestTT $ TestList testCases
             