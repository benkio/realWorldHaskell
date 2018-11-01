module PrettyStub where

import SimpleJSON

data Doc =  ToBeDefined
  deriving (Show)

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

double :: Double -> Doc
double num = undefined

text :: String -> Doc
text str = undefined

enclose :: Char -> Char -> Doc -> Doc
enclose rightChar leftChar doc = undefined

hcat :: [Doc] -> Doc
hcat ds = undefined

oneChar :: Char -> Doc
oneChar x = undefined