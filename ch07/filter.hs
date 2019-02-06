
main :: IO ()
main = interact (unlines . filter (elem 'a') . lines)