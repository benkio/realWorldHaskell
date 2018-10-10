suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs :suffixes xs'
suffixes _ = []

noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) :suffixes xs
noAsPattern _ = []