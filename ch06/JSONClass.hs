{-# LANGUAGE FlexibleInstances #-}

module JSONClass(
  JAry(fromJAry),
  jary,
  JObj(fromJObj),
  jobj,
  doubleToJValue
  ) where

import Data.Either
import Control.Arrow (second)

type JSONError = String
newtype JAry a = JAry {
  fromJAry :: [a]
  } deriving (Eq, Ord, Show)
newtype JObj a = JObj { fromJObj :: [(String, a)] } deriving (Ord, Eq, Show)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)   -- was [(String, JValue)]
            | JArray (JAry JValue)    -- was [JValue]
              deriving (Eq, Ord, Show)

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool a) = Right a
    fromJValue _ = Left "Not a JSON Boolean"

instance JSON String where
    toJValue = JString
    fromJValue (JString a) = Right a
    fromJValue _ = Left "not a JSON String"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id

instance JSON a => JSON (JAry a) where
  toJValue = jaryToJValue
  fromJValue = jaryFromJValue

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObj

    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
      where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "not a JSON object"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber n) = Right (f n)
doubleToJValue _ _ = Left "Not a JSON Number"

jary :: [a] -> JAry a
jary = JAry

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "Error: Impossible to convert to JAry"

jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue =  jaryOfJValuesToJValue . jary . listToJValues . fromJAry

jobj :: [(String, a)] -> JObj a
jobj = JObj

listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left x) = Left x
whenRight f (Right x) = Right (f x)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers _ [] = Right []
mapEithers f (x:xs) = do
  c <- f x
  cs <- mapEithers f xs
  return (c:cs)
