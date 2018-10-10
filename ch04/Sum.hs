
mySum xs = helper 0 xs
           where helper acc (x:xs) = helper (acc + x) xs
                 helper acc [] = acc


mySum' = foldl (+) 0

niceSum :: [Integer] -> Integer
niceSum = mySum'
