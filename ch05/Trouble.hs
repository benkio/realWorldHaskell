import Data.Char (toUpper)

upcaseFirst (c:cs) = toUpper c -- forgot ":cs" here

camelCase :: String -> String
camelCase xs = concat $ map upcaseFirst (words xs)
