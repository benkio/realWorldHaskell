
myFoldl :: [a] -> b -> (b -> a -> b) -> b
myFoldl (x:xs) z f = myFoldl xs (f z x) f
myFoldl [] z _ = z

myFoldr :: [a] -> b -> (a -> b -> b) -> b
myFoldr [] z _ = z
myFoldr (x:xs) z f = f x (myFoldr xs z f)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p []   = []
myFilter p (x:xs)
    | p x       = x : myFilter p xs
    | otherwise = myFilter p xs

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' p xs = myFoldr xs [] (\x acc -> if p x then (acc ++ [x]) else acc)

myMap :: [a] -> (a -> b) -> [b]
myMap xs f = foldr (\a b -> f a : b) [] xs

myFoldl' :: (b -> a -> b) -> b -> [a] -> b
myFoldl' f z xs = foldr step id xs z
  where step x g a = g (f a x)

identity :: [a] -> [a]
identity xs = foldr (:) [] xs

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

-- foldl evaluation, all the computation is stored
-- foldl (+) 0 (1:2:3:[])
--           == foldl (+) (0 + 1)             (2:3:[])
--           == foldl (+) ((0 + 1) + 2)       (3:[])
--           == foldl (+) (((0 + 1) + 2) + 3) []
--           ==           (((0 + 1) + 2) + 3)

foldl' _ zero [] = zero
foldl' step zero (x:xs) =
  let new = step zero x
  in new `seq` foldl' step new xs

-- incorrect: seq is hidden by the application of someFunc
-- since someFunc will be evaluated first, seq may occur too late
-- hiddenInside x y = someFunc (x `seq` y)

-- incorrect: a variation of the above mistake
-- hiddenByLet x y z = let a = x `seq` someFunc y
--                     in anotherFunc a z

-- -- correct: seq will be evaluated first, forcing evaluation of x
-- onTheOutside x y = x `seq` someFunc y

-- evaluates x and y first.
-- chained x y z = x `seq` y `seq` someFunc z

badExpression step zero (x:xs) =
  seq (step zero x)
      (badExpression step (step zero x) xs)

strictPair (a,b) = a `seq` b `seq` (a,b)

strictList (x:xs) = x `seq` x : strictList xs
strictList []     = []