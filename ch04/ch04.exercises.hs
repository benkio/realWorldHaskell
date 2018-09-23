myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f xs = foldl (\b a -> f a || b) False xs