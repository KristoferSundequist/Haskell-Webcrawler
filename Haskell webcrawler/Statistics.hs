{-

These are functions related to statistics.

All files name StXXX are external modules related to this site. The may not fully commented or have tests.
This doesn't matter for our purposes since they are examples of data that a user would supply.

-}

module Statistics where 

import qualified Text.HTML.TagSoup as TagSoup
import qualified Parser as P

{-  Representation Convention: A statistical function. Consists of a tuple with two functions.
                               The first function is a function which takes HTML and returns some statistic about it.
                               The second function is the means to combine results from the first function.
    Representation Invariant:  True
-}
data StatFn statType = StatFn ( (P.TagList -> statType), (statType -> statType -> statType) )

-- A sample list of statistics functions to provide for the crawler
statFnSamples1 :: [StatFn (String, Int)]
statFnSamples1 = [statFn1]

-- sample statistics functions for the crawler

-- counts number of links on a site
statFn1 :: StatFn (String, Int)
statFn1 = StatFn  (fnA, fnB)
  where
    fnA tagList = ("Link count:", length $ P.getUrls tagList)
    fnB a b = (fst a, snd a + snd b)
