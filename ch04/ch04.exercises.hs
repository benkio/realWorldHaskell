myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f xs = foldl (\b a -> f a || b) False xs

myCycle :: [a] -> [a]
myCycle xs = foldr (:) (myCycle xs) xs

myWords :: String -> [String]
myWords phrase = filter (/= "") $ foldl step [""] phrase
  where
    step :: [[Char]] -> Char -> [[Char]]
    step acc c
      | (last acc) == "" && (c == ' ') = acc
      | c == ' ' || c == '\n' = acc ++ [""]
      | last acc == "" = acc ++ [""++[c]]
      | otherwise = (init acc) ++ [(last acc) ++ (""++[c])]

myUnlines :: [String] -> String
myUnlines xs = foldr (\a b -> a ++ ['\n'] ++ b ) "" xs