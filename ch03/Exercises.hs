import Data.Ord
import Data.List

myLength :: [a] -> Int
myLength [] = 0
myLength xs = internalCount 0 xs
              where
                internalCount a [] = a
                internalCount a ls = internalCount (a+1) (tail ls)

myMean [] = 0
myMean xs = div (sum xs) (length xs)

myPalindrome [] = []
myPalindrome xs = xs ++ (reverse xs)

myIsPalindrome [] = True
myIsPalindrome xs
  | (length xs) `mod` 2 /= 0 = False
  | otherwise = firstHalf == (reverse secondHalf)
                where
                  (firstHalf, secondHalf) = splitAt (div (length xs) 2) xs

mySortListOfList :: [[a]] -> [[a]]
mySortListOfList xxs = sortBy (\xs ys -> (length xs) `compare` (length ys)) xxs