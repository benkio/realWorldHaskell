import Data.Ord
import Data.List
import Data.List.Split

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

data Direction = LeftTurn | RightTurn | Straight
               deriving (Show, Eq)

type TwoDimensionPoint = (Int, Int)

calculateTurn :: TwoDimensionPoint -> TwoDimensionPoint -> TwoDimensionPoint -> Direction
calculateTurn (x1, y1) (x2, y2) (x3, y3)
  | crossProduct == 0 = Straight
  | crossProduct > 0  = LeftTurn
  | crossProduct < 0  = RightTurn
    where crossProduct = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

calculateTurns :: [TwoDimensionPoint] -> [Direction]
calculateTurns (x:y:z:xs) = (calculateTurn x y z) : (calculateTurns (y:z:xs))
calculateTurns _ = []

grahamScan :: [TwoDimensionPoint] -> [TwoDimensionPoint]
grahamScan xs = let
  startingPoint = minimumBy (\x y -> (snd x) `compare` (snd y)) xs
  xsSorted = startingPoint : (sortBy (compareFrom startingPoint) (filter (/= startingPoint) xs))
  in grahamScan' (drop 2 xsSorted) (take 2 xsSorted)

grahamScan' :: [TwoDimensionPoint] -> [TwoDimensionPoint] -> [TwoDimensionPoint]
grahamScan' [] acc = acc
grahamScan' xs acc = case (calculateTurn beforelastAccepted lastAccepted (head xs)) of
                       RightTurn -> grahamScan' xs (init acc)
                       _         -> grahamScan' (tail xs) (acc ++ [(head xs)])
  where
    lastAccepted = head (reverse acc)
    beforelastAccepted = last (take 2 (reverse acc))

compareFrom :: TwoDimensionPoint -> TwoDimensionPoint -> TwoDimensionPoint -> Ordering
compareFrom (ox, oy) (lx,ly) (rx, ry) =
  compare ((lx - ox) * (ry - oy)) ((ly - oy) * (rx - ox))
