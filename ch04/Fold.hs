
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