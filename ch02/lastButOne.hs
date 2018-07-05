-- file: ch02/lastButOne.hs
lastButOne xs
  | length xs < 2  = error "cannot extract last but one character"
  | length xs == 2 = head xs
  | otherwise      = lastButOne (tail xs)
  