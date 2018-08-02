import System.Environment (getArgs)
import Data.Maybe
import Data.Text (transpose, unpack, pack)

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

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = wordsTrasposed

firstWords :: String -> String
firstWords = unwords . (>>= (maybeToList . safeHead . words)) . lines

wordsTrasposed :: String -> String
wordsTrasposed = unwords . (map unpack) . transpose . (map pack) . words