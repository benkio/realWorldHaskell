
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