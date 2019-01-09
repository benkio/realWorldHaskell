main = do
  putStrLn "Please enter a Double"
  input <- getLine
  let inputConverted = (read input)::Double
  putStrLn("Twice " ++ show inputConverted ++ " is " ++ show (inputConverted * 2))
