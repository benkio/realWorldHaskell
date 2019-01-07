module Prettify where

import Prelude hiding ((<>))

{- STUBS -}

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

compact = undefined
pretty = undefined

{- END OF STUBS -}

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
