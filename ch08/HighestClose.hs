import qualified Data.ByteString.Lazy.Char8 as L

closing :: L.ByteString -> Maybe Int
closing = readPrice . (!!4) . L.split ','

readPrice :: L.ByteString -> Maybe Int
readPrice price = do
  (dollars, rest) <- L.readInt price
  (cents, _) <- L.readInt (L.tail rest)
  return $ dollars * 100 + cents

highestClose :: L.ByteString -> Maybe Int
highestClose = maximum . (Nothing:) . map closing . L.lines

highestCloseFrom :: FilePath -> IO ()
highestCloseFrom fp = do
  contents <- L.readFile fp
  print $ highestClose contents