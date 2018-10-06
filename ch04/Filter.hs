

oddFilter :: [Int] -> [Int]
oddFilter (x:xs)
  | x `mod` 2 == 0 = oddFilter xs
  | otherwise = x : oddFilter xs
oddFilter [] = []

oddFilter' :: [Int] -> [Int]
oddFilter' xs = filter odd xs