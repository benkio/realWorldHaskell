
main :: IO ()
main =
  putStrLn "Greetings! What is your name?" >>
  getLine >>=
  (\x -> putStrLn ("Welcome to Halkell " ++ x ++ "!!!"))