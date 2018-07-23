import Data.Ord
import Data.List

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)


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

myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = []
myIntersperse _ (x:[]) = x
myIntersperse x (ys:yys) = (ys++[x]) ++ (myIntersperse x yys)

treeHopCount :: Tree a -> Int
treeHopCount Empty = 0
treeHopCount (Node x r l) = 1 + (max (treeHopCount r) (treeHopCount l) )

Data Direction = Left
               | Right
               | Straight
               deriving (Show)

type 2DPoint = (Int, Int)

calculateTurn :: 2DPoint -> 2DPoint -> 2DPoint -> Direction
calculateTurn (x1, y1) (x2, y2) (x3, y3) =