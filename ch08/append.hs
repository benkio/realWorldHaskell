module CH08 where

import Prelude hiding ((++))

(++) :: [a] -> [a] -> [a]
(x:xs) ++ ys = x : (xs ++ ys)
[] ++ ys = ys
