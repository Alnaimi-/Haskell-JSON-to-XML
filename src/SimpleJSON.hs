module SimpleJSON
    (
      JValue(..)
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    ) where


data JValue = JString (String,String)
            | JNumber (String,Float)
            | JBool (String,Bool)
            | JNull (String)
            | JObject (String,[JValue])
            | JArray (String,[JValue])
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe (String,String)
getString (JString s) = Just s
getString _ = Nothing

getInt (JNumber n) = Just (truncate $ snd n)
getInt _ = Nothing

getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool (JBool b) = Just b
getBool _ = Nothing

getObject (JObject o) = Just o
getObject _ = Nothing

getArray (JArray a) = Just a
getArray _ = Nothing
