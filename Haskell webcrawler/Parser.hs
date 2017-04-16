module Parser where

-- since this is the parser it's not necessary to import qualified
import Data.Char
import Data.Maybe (fromJust)
import Network.URI
import qualified Text.HTML.TagSoup as TagSoup
import Data.List (isPrefixOf, isInfixOf)
import Test.HUnit

{- REPRESENTATION CONVENTION: A list of tags represented by a list of the type Tag String (imported from Text.HTML.TagSoup)
   REPRESENTATION INVARIANT: True
-}
type TagList = [TagSoup.Tag String]
{- REPRESENTATION CONVENTION: A key value represented as a string
   REPRESENTATION INVARIANT: True
-}
type Key = String

{- htmlToTagList
  PURPOSE: Binds a chosen parsing function from an external library (for example parseTags from Text.HTML.TagSoup) 
    to a handle so that it can be exported with the module
-}
htmlToTagList :: (String -> TagList)
htmlToTagList = TagSoup.parseTags

{- getUrls tagList
   Purpose:  Gets all URLs which are under the href attribute of opening a-tags in tagList.
   Pre:      True
   Post:     A list of all URLs that were found under the href attribute of opening a-tags in tagList.
   Examples: getUrls [TagSoup.TagOpen "a" [("href","url1")]] == ["url1"]
             getUrls [TagSoup.TagOpen "a" [("foo", "bar")]] == [""]
             getUrls [TagSoup.TagOpen "a" [("href","url1"), ("href","badurl")],TagSoup.TagText "foo",TagSoup.TagClose "a",
                      TagSoup.TagOpen "a" [("href","url2"),("title","bar")],TagSoup.TagClose "a"] == ["url1", "url2"]
-}
getUrls :: TagList -> [String]
getUrls tagList = map (TagSoup.fromAttrib "href") $ filter (TagSoup.~== TagSoup.TagOpen "a" []) tagList


{- relativeToAbsolute uri url
	 Purpose: Turns url into absolute by adding the scheme and regname of uri to it if needed.
	 Pre:     True
	 Post:    An absolute link based on url and components from uri
   Variant: isRelative (will be false after 1 recursion)
	 Example:
            relativeToAbsolute (URI "http:" (Just (URIAuth "" "www.uu.se" "")) "" "" "") "/it/" == "http://www.uu.se/it/"
            relativeToAbsolute (URI "http:" (Just (URIAuth "" "www.uu.se" "")) "" "" "") "it/"  == "http://www.uu.se/it/"
            relativeToAbsolute (URI "http:" (Just (URIAuth "" "www.uu.se" "")) "" "" "") "it"   == "http://www.uu.se/it"
            relativeToAbsolute (URI "http:" (Just (URIAuth "" "www.uu.se" "")) "" "" "") "www.uu.se/it" == "http://www.uu.se/it"
            relativeToAbsolute (URI "" (Just (URIAuth "" "" "")) "" "" "") "www.uu.se" == "www.uu.se"
-}
relativeToAbsolute :: URI -> String -> String
relativeToAbsolute uri url | isRelative    = relativeToAbsolute uri $ uriRegName ++ "/" ++ (if (head url) == '/' then tail url else url)
                           | addScheme = (uriScheme uri) ++ "//" ++ url
                           | otherwise     = url
  where
    isRelative =    not (isInfixOf  uriRegName urlInLower)
                 && not (isPrefixOf "www."     urlInLower)
                 && isRelativeReference url
    addScheme  = not (isPrefixOf "http://" url) && uriScheme uri /= ""  
    urlInLower = map toLower url
    uriRegName = getRegName uri

{-  normalizeUri uri
    Purpose: Normalize uri by converting its fragment field to an empty string 
             and converting every other field except query to lower case.
    Pre: True
    Post: A normalized version of uri.
    Examples: 
              normalizeUri (URI "Scheme" (Just (URIAuth "" "Www.Regn.Com" "")) "Sno" "QueRy" "fragment") == 
               (URI "scheme" ( Just (URIAuth "" "www.regn.com" "")) "sno" "QueRy" "")

-}
normalizeUri :: URI -> URI
normalizeUri (URI scheme auth path query frag) = URI (low scheme) (fmap lowAuth auth) (low path) query ""
	where
  	low s = map (toLower) s
  	lowAuth (URIAuth u r p) = URIAuth (low u) (low r) (low p)

{- urlsToUris urls
   Purpose:  Convert every url in urls to a normalized uri
   Pre: True
   Post: URIs corresponding to every url in urls
   Examples:
    urlsToUris ["http://www.uu.se/test/", "not a url"] == 
     [URI "http:" (Just (URIAuth "" "www.uu.se" "")) "/test/" "" ""]

-}
urlsToUris :: [String] -> [URI]
urlsToUris links = map (fromJust . (fmap normalizeUri)) (filter (\x -> x /= Nothing && (uriScheme (fromJust x) == "http:")) (map parseURI (map addSlash links)))
	where
  	addSlash :: String -> String
  	addSlash url = if last url == '/' then url else url ++ "/"


{- getInternalUris uris site
  Purpose: Remove every uri in uris that does not have regName equal to site
  Pre: True
  Post: Every uri in uris that has (regName uri == site)

  Examples: getInternalUris [(URI "a" (Just (URIAuth "b" "regn" "d")) "e" "f" "g"), (URI "a" (Just (URIAuth "b" "solsken" "d")) "e" "f" "g")]  "regn" 
                              == [(URI "a" (Just (URIAuth "b" "regn" "d")) "e" "f" "g")]
-}
getInternalUris :: [URI] -> String -> [URI]
getInternalUris uris urlRegName = [x | x <- uris, getRegName x == urlRegName]

{- getRegName uri
   Purpose:  Returns the regname from a uri.
   Pre:      True.
   Post:     A string representing the regname. If URIAuth == Nothing it returns "".
   Examples: getRegName (URI "a" (Just (URIAuth "b" "regn" "d")) "e" "f" "g") == "regn"
             getRegName (URI "a" (Just (URIAuth "b" ""     "d")) "e" "f" "g") == ""
             getRegName (URI "a"  Nothing                        "e" "f" "g") == ""
-}
getRegName :: URI -> String
getRegName x | auth == Nothing = "" -- Possible TODO: This implementation might not be preferable.
             | otherwise       = uriRegName $ fromJust auth
  where
    auth = uriAuthority x

{- uriToKey uri
   Purpose:  Generates a key from uri.
   Pre:      True
   Post:     uri regname ++ uri path ++ uri query
   Examples: uriToKey (URI "http:" (Just (URIAuth "" "www.uu.se" "")) "/pizza/" "?imgGallery" "") == "www.uu.se/pizza/?imgGallery"
-}
uriToKey :: URI -> Key
uriToKey uri = getRegName nUri ++ uriPath nUri ++ uriQuery nUri
  where
    nUri = normalizeUri uri

{- uriToUrl
   Purpose:  Gets a normalized url out of a uri.
   Pre:      True
   Post:     uri scheme ++ uri regname ++ uri path ++ uri query
   Examples: uriToUrl (URI "a" (Just (URIAuth "b" "regn" "d")) "e" "f" "g") == "a//regnef/"
             uriToUrl (URI "a"  Nothing                        "e" "f" "g") == "a//ef/"
-}
uriToUrl :: URI -> String
uriToUrl uri = url ++ (if last url == '/' then "" else "/")
  where
    url  = uriScheme nUri ++ "//" ++ getRegName nUri ++  uriPath nUri ++ uriQuery nUri
    nUri = normalizeUri uri

{- urlToUri url
   Purpose:  Normalize and convert a url to a uri.
   Pre:      Network.URI.isURI url
   Post:     A URI based on a normalized version of the url.

   Examples: Valid:
             urlToUri "foo:Bar" == URI "foo" Nothing "bar" "" ""
             Invalid:
             urlToUri ":bar"       Pre-condition not satisfied
             urlToUri "bar"        Pre-condition not satisfied
-}
urlToUri :: String -> URI
urlToUri url = fromJust $ fmap normalizeUri $ parseURI url

-- tests
testCases :: [Test]
testCases = [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18]

--getUrls tests
test1 =  TestCase $ assertBool ("Parser: getUrls 1") $ getUrls [TagSoup.TagOpen "a" [("href","url1")]] == ["url1"]
test2 =  TestCase $ assertBool ("Parser: getUrls 2") $ 
          getUrls [TagSoup.TagOpen "a" [("href","url1"), ("href","badurl")],TagSoup.TagText "foo",TagSoup.TagClose "a",TagSoup.TagOpen "a" [("href","url2"),("title","bar")],TagSoup.TagClose "a"] 
            == ["url1", "url2"]
test3 = TestCase $ assertBool ("Parser: getUrls 3") $ getUrls [TagSoup.TagOpen "a" [("foo", "bar")]] == [""]

--relativeToAbsolute tests
test4 = TestCase $ assertBool ("Parser: relativeToAbsolute 1") $ 
         relativeToAbsolute (URI "http:" (Just (URIAuth "" "www.uu.se" "")) "" "" "") "/it/" == "http://www.uu.se/it/"
test5 = TestCase $ assertBool ("Parser: relativeToAbsolute 2") $
         relativeToAbsolute (URI "http:" (Just (URIAuth "" "www.uu.se" "")) "" "" "") "it/"  == "http://www.uu.se/it/"
test6 = TestCase $ assertBool ("Parser: relativeToAbsolute 3") $
         relativeToAbsolute (URI "http:" (Just (URIAuth "" "www.uu.se" "")) "" "" "") "it"   == "http://www.uu.se/it"
test7 = TestCase $ assertBool ("Parser: relativeToAbsolute 4") $ 
         relativeToAbsolute (URI "http:" (Just (URIAuth "" "www.uu.se" "")) "" "" "") "www.uu.se/it" == "http://www.uu.se/it"
test8 = TestCase $ assertBool ("Parser: relativeToAbsolute 5") $
         relativeToAbsolute (URI "" (Just (URIAuth "" "" "")) "" "" "") "www.uu.se" == "www.uu.se"

--urlsToUris tests 
test9 = TestCase $ assertBool ("Parser: urlsToUris") $ 
         urlsToUris ["http://www.uu.se/test/", "not a url"] == [URI "http:" (Just (URIAuth "" "www.uu.se" "")) "/test/" "" ""]
--normalizeUri tests
test10 = TestCase $ assertBool ("Parser: normalizeUri") $ 
          normalizeUri (URI "Scheme" (Just (URIAuth "" "Www.Regn.Com" "")) "Sno" "QueRy" "fragment") == 
           (URI "scheme" ( Just (URIAuth "" "www.regn.com" "")) "sno" "QueRy" "")

test11 = TestCase $ assertBool ("Parser: getInternalUris") $ 
                     getInternalUris [(URI "a" (Just (URIAuth "b" "regn" "d")) "e" "f" "g"), (URI "a" (Just (URIAuth "b" "solsken" "d")) "e" "f" "g")]  "regn" == 
                      [(URI "a" (Just (URIAuth "b" "regn" "d")) "e" "f" "g")]
--getRegName tests
test12 = TestCase $ assertBool ("Parser: getRegName 1") $ getRegName (URI "a" (Just (URIAuth "b" "regn" "d")) "e" "f" "g") == "regn"
test13 = TestCase $ assertBool ("Parser: getRegName 2") $ getRegName (URI "a" (Just (URIAuth "b" ""     "d")) "e" "f" "g") == ""
test14 = TestCase $ assertBool ("Parser: getRegName 3") $ getRegName (URI "a"  Nothing                        "e" "f" "g") == ""
--uriToKey tests
test15 = TestCase $ assertBool ("Parser: uriToKey") $ uriToKey (URI "http:" (Just (URIAuth "" "www.uu.se" "")) "/pizza/" "?imgGallery" "") == "www.uu.se/pizza/?imgGallery"
--uriToUrl tests
test16 = TestCase $ assertBool ("Parser: uriToUrl 1") $ uriToUrl (URI "a" (Just (URIAuth "b" "regn" "d")) "e" "f" "g") == "a//regnef/"
test17 = TestCase $ assertBool ("Parser: uriToUrl 2") $ uriToUrl (URI "a"  Nothing "e" "f" "g") == "a//ef/"
--urlToUri tests
test18 = TestCase $ assertBool ("Parser: urltoUri") $ urlToUri "foo:Bar" == URI "foo:" Nothing "bar" "" ""
