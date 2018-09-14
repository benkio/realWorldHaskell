
myFoldl :: [a] -> b -> (a -> b -> b) -> b
myFoldl (x:xs) z f = myFoldl xs (f x z) f
myFoldl [] z _ = z