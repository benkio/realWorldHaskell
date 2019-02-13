
import Data.Char(toUpper)

isGreen :: IO Bool
isGreen = do
  putStrLn "Is Green your favorite color?"
  input <- getLine
  return ((toUpper . head $ input) == 'Y')