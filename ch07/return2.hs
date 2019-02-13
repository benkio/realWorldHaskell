
import Data.Char(toUpper)

isGreen :: IO Bool
isGreen = do
  putStrLn "Is Green your favorite color?"
  input <- getLine
  return (isYes input)

isYes :: String -> Bool
isYes s = (toUpper . head $ s) == 'Y'