
mySum xs = helper 0 xs
           where helper acc (x:xs) = helper (acc + x) xs
                 helper acc [] = acc


mySum' xs = foldl (+) 0