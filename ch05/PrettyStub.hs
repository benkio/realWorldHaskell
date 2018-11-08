module PrettyStub where

import SimpleJSON

data Doc =  ToBeDefined
  deriving (Show)

double :: Double -> Doc
double num = undefined

text :: String -> Doc
text str = undefined

hcat :: [Doc] -> Doc
hcat ds = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

fsep :: [Doc] -> Doc
fsep docs = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate doc docs = undefined