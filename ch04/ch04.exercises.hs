safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast xs = safeHead $ reverse xs

safeInit :: [a] -> Maybe [a]
safeInit xs = fmap (reverse) (safeTail (reverse xs))

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs =
  let (first, second) = span f xs
  in first : splitWith (not . f) second