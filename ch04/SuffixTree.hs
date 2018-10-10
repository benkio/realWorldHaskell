import Data.ByteString (tails, ByteString)

suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs :suffixes xs'
suffixes _ = []

noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) :suffixes xs
noAsPattern _ = []

suffixes2 :: ByteString -> [ByteString]
suffixes2 xs = init (tails xs)

compose :: (b -> d) -> (a -> b) -> a -> d
compose f g a = f (g a)

suffixes3 :: ByteString -> [ByteString]
suffixes3 xs = compose init tails xs

suffixes4 :: ByteString -> [ByteString]
suffixes4 = compose init tails

suffixes5 :: ByteString -> [ByteString]
suffixes5 = init . tails