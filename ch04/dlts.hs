import Data.List (isPrefixOf)

dlts :: String -> [String]
dlts = foldr step [] . lines
  where
    step l ds
      | "#define DLT" `isPrefixOf` l = secondWord l : ds
      | otherwise = ds
    secondWord = head . tail . words