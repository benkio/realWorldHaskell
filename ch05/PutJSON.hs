module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber b) = show b
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"
renderJValue (JObject o) = "{"++ pair o ++ "}"
  where
    pair [] = ""
    pair os = intercalate ", " (map pairRender os)
    pairRender (k, v) = show k ++ ":" ++ renderJValue v
renderJValue (JArray xs) = "[" ++ renderElems xs  ++ "]"
  where
    renderElems [] = ""
    renderElems ys = intercalate ", " (map renderJValue ys)

putJValue :: JValue -> IO ()
putJValue jv = putStrLn (renderJValue jv)