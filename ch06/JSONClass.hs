{-# LANGUAGE FlexibleInstances #-}

import SimpleJSON

type JSONError = String

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

instance JSON [a] where
    toJValue = undefined
    fromJValue = undefined

instance (JSON a) => JSON [(String, a)] where
    toJValue = undefined
    fromJValue = undefined
    
doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber n) = Right (f n)
doubleToJValue _ _ = Left "Not a JSON Number"