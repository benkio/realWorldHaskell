import Data.Char (toUpper)

square :: [Double] -> [Double]
square xs = (\x -> x * x) `map` xs

square' :: [Double] -> [Double]
square' (x:xs) = x*x : square' xs
square' [] = []

uppercase :: String -> String
uppercase (x:xs) = toUpper x : uppercase xs
uppercase [] = []

uppercase' :: String -> String
uppercase' = map toUpper

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs