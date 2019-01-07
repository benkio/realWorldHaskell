module Prettify where

import Prelude hiding ((<>))

{- STUBS -}
compact = undefined
pretty = undefined

{- END OF STUBS -}

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show,Eq)

double :: Double -> Doc
double num = text (show num)

text :: String -> Doc
text "" = Empty
text s = Text s

char :: Char -> Doc
char c = Char c

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
a <> Empty = a
Empty <> b = b
a <> b = a `Concat` b

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr (f) Empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
