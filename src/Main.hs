module Main where

{--TODO
	finish typeCheck method which creates object type and passes that section of string
	create a method which seperates arrays and orjects into base pairs (then calls typecheck on these JValues) 
-}

import SimpleJSON
import Data.Char

{-main = do
       file <- readFile "text.json"
       as = filter notWhiteSpace file-}


removeQuot x = init $ tail x
getName x = removeQuot $ takeWhile notColon x -- extract name from string
dropName x = tail $ dropWhile notColon x  -- tail to remove colon from returned string 
notColon x = not $ x == ':'


headChecker x y = (head (dropName x)) == y -- check the head of the value to see what type it is
numberChecker x = isDigit (head (dropName x)) -- check if head is a digit 

parse :: String -> JValue
parse [] = JNull ("temp")
parse (x:xs) | x == '{'  = parseObject ("\"root\":"++(x:xs))
             | x == '['  = parseArray ("\"root\":"++(x:xs))
             | x == '\"' = typeCheck (x:xs)

typeCheck :: String -> JValue
typeCheck x | (headChecker x '\"') = parseString x
            | (headChecker x 'n')  = parseNull x
            | numberChecker x = parseNumber x
            | (headChecker x 't') || (headChecker x 'f')  = parseBool x
            | (headChecker x '{') = parseObject x 
            | otherwise = JNull [(head (dropName x))] 


parseNull :: String -> JValue
parseNull x = JNull (getName x)

parseString:: String -> JValue
parseString x = JString ((getName x), (removeQuot $ dropName x))

parseNumber:: String -> JValue
parseNumber x = JNumber ((getName x), read (dropName x)::Float)

parseBool :: String -> JValue
parseBool x = JBool ((getName x), boolChecker $ dropName x)
               where boolChecker x = (if head x == 't' then True else False)

parseObject :: String -> JValue
parseObject x = JObject ((getName x), makelist x )

objectFromString:: String -> String
objectFromString x = tail$ take (extract (tail $ dropName x)) (dropName x)

makelist:: String -> [JValue]
makelist x = map typeCheck (splitObject $ objectFromString x)

----------------------------- helper methods 


objectExtractor:: Int -> String -> Int -> Int
objectExtractor 0 string final = final
objectExtractor count (st:strs) final | st == '{' = (objectExtractor (count+1) strs (final+1))
                                      | st == '}' = (objectExtractor (count-1) strs (final+1))
                                      | otherwise = (objectExtractor count strs (final+1))
objectExtractor count [] final = error "The JSON broke"
extract x = objectExtractor 1 x 0 

--------------------------
splitObject:: String -> [String]
splitObject x | length x < 3 = []
              | otherwise = let first = (firstPair x) in first : (splitObject $ drop (length first +1) x)

firstPair:: String -> String
firstPair x = "\"" ++ (getName x) ++ "\"" ++ ":" ++ (helper $ dropName x) --need to work out why one qot needed

helper [] = []
helper (x:xs) | x == '{' = '{':(take (extract xs) xs) 
              | notFinish x = x:(helper xs)
              | otherwise = []
                 where notFinish x = (not (x == ',')) && (not (x == '}'))


-------------------------------



parseArray :: String -> JValue
parseArray x = JNull ("temp")


testString = "\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},"
testString3 = "\"id\":\"file\""
testString4 = "\"id\":null"
testString5 = "\"id\":100.203"
testString6 = "\"id\":true"
testString7 = "\"id\":false"
testString8 = "{\"value\":\"New\",\"onclick\":\"CreateNewDoc()\"}"
testString9 = "\"value\":\"New\",\"onclick\":\"CreateNewDoc()\",\"node\":{\"value\":\"Open\",\"onclick\":\"OpenDoc()\"}"
testString10 = "{\"value\":\"New\",\"onclick\":\"CreateNewDoc()\",\"node\":{\"value\":\"Open\",\"onclick\":\"OpenDoc()\"}}"
testString11 = "\"node\":{\"value\": \"Open\", \"onclick\": \"OpenDoc()\"}"
testString12 = "{\"value\":\"New\",\"onclick\":\"CreateNewDoc()\",\"node\":{\"value\":\"Open\",\"onclick\":\"OpenDoc()\"},\"numTest\":102.20,\"BoolTest\":true,\"nullTest\":null}"

--testString2 = "\"menuitem\": [{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},{\"value\":
-- \"Open\", \"onclick\": \"OpenDoc()\"},{\"value\": \"Close\", \"onclick\": \"CloseDoc()\"}]}"

trim :: [Char] -> [Char]
trim [] = []
trim (x:xs) | elem x [' ', '\t', '\n'] = trim xs
            | otherwise = x : trim xs