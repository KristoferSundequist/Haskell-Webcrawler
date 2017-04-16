{-

These are functions related to statistics.

All files name StXXX are external modules related to this site. The may not fully commented or have tests.
This doesn't matter for our purposes since they are examples of data that a user would supply.

-}

module Statistics where 

import qualified Text.HTML.TagSoup as TagSoup
import qualified Parser as P
import qualified StCompLing as StCompLing

{-  Representation Convention: A statistical function. Consists of a tuple with two functions.
                               The first function is a function which takes HTML and returns some statistic about it.
                               The second function is the means to combine results from the first function.
    Representation Invariant:  True
-}
data StatFn statType = StatFn ( (P.TagList -> statType), (statType -> statType -> statType) )

-- A sample list of statistics functions to provide for the crawler
statFnSamples1 :: [StatFn (String, Int)]
statFnSamples1 = [statFn2]

-- sample statistics functions for the crawler

-- counts number of links on a site
statFn1 :: StatFn (String, Int)
statFn1 = StatFn  (fnA, fnB)
  where
    fnA tagList = ("Link count:", length $ P.getUrls tagList)
    fnB a b = (fst a, snd a + snd b)


{- statFn2. Another sample statistics function. It's very slow and we don't have time to optimize it,
            don't use it in statFnSamples1. This function uses StCompLing.

   Purpose: Finds the top 10 wordcounts of site represented by a taglist.
            fnA sums the top 10 counts.
            fnB combined the toplists into a new toplist, returning a new top 10 wordcount.
  -}
statFn2 :: StatFn (String, Int)
statFn2 = StatFn (fnA, fnB)
  where
    fnA tagList = (show wordCount,0)
      where
        doc :: StCompLing.Document
        doc = foldl (++) [] $ map (words . TagSoup.fromTagText) $ filter TagSoup.isTagText tagList
        filteredDoc = statFn2FilterCommonWords doc
        wordCount = take 10 $ StCompLing.sortTallyCountHL $ StCompLing.wordCount filteredDoc
    
    fnB a b = (show combinedTally, 0)
      where
        combinedTally = take 10 $ StCompLing.sortTallyCountHL $ StCompLing.combineWordTallies wordTallyA wordTallyB
        wordTallyA = read $ fst a
        wordTallyB = read $ fst b

-- The function which removes common words:
--statFn2FilterCommonWords = StCompLing.filterEnglish
statFn2FilterCommonWords = StCompLing.filterSwedish
