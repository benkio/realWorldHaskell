import System.Environment (getArgs)
import Data.Char
import Data.Maybe
import Data.Text (transpose, unpack, pack)
import qualified Control.Exception as E

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

-- Fold exercises --------------------------------------------

asInt_fold :: String -> Int
asInt_fold ('-':xs) = negate $ asInt_fold xs
asInt_fold n = fst $ foldr composeFunc (0, 0) n
  where composeFunc d (acc, pos)
          | isDigit d = (acc + (10 ^ pos) * digitToInt d, pos + 1)
          | otherwise = error "input is not a digit"

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either ('-':xs) = fmap negate $ asInt_either xs
asInt_either n = fmap fst $ foldr composeFunc (Right (0, 0)) n
  where
    composeFunc _ (Left message) = Left message
    composeFunc d (Right(acc, pos))
          | isDigit d = Right (acc + (10 ^ pos) * digitToInt d, pos + 1)
          | otherwise = Left "input is not a digit"

concat :: [[a]] -> [a]
concat xss = foldr (\xs accs -> xs ++ accs) [] xss

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs)
  | f x = x : myTakeWhile f xs
  | otherwise = []

myTakeWhile' :: (a -> Bool) -> [a] -> [a]
myTakeWhile' f xs = foldr (\a acc -> if f a then a : acc else []) [] xs

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f (x:xs) = foldl step [[x]] xs
  where
         step acc x
               | f lastElem x      = (init acc) ++ [lastList ++ [x]]
               | otherwise         = acc ++ [[x]]
               where lastList = last acc
                     lastElem = last lastList

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f xs = foldl (\b a -> f a || b) False xs

myCycle :: [a] -> [a]
myCycle xs = foldr (:) (myCycle xs) xs

myWords :: String -> [String]
myWords phrase = filter (/= "") $ foldl step [""] phrase
  where
    step :: [[Char]] -> Char -> [[Char]]
    step acc c
      | (last acc) == "" && (c == ' ') = acc
      | c == ' ' || c == '\n' = acc ++ [""]
      | last acc == "" = acc ++ [""++[c]]
      | otherwise = (init acc) ++ [(last acc) ++ (""++[c])]

myUnlines :: [String] -> String
myUnlines xs = foldr (\a b -> a ++ ['\n'] ++ b ) "" xs
