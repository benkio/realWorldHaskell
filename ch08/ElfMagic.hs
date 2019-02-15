
import qualified Data.ByteString.Lazy as L

hasElfMagic :: L.ByteString -> Bool
hasElfMagic bs = L.take 4 bs == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isEflFile :: FilePath -> IO Bool
isEflFile path = do
  content <- L.readFile path
  return (hasElfMagic content)