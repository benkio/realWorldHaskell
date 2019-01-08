module Prettify where

import Prelude hiding ((<>))

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
fold f = foldr f Empty

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

compact :: Doc -> String
compact doc = transform [doc]
  where transform [] = ""
        transform (d:ds) =
          case d of
            Empty -> transform ds
            Char c -> c : transform ds
            Text s -> s ++ transform ds
            Line -> '\n' : transform ds
            Concat d1 d2 -> transform(d1:d2:ds)
            Union _ d2 -> transform(d2:ds)

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
_ `fits` ""        = True
_ `fits` ('\n':_)  = True
w `fits` (_:cs)    = (w - 1) `fits` cs

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""
          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

fill :: Int -> Doc -> Doc
fill limit doc
  | limit `fits` docPretty = fillSpaces (limit - (length docPretty)) doc
  | otherwise = doc
    where docPretty = (pretty (maxBound :: Int) doc)
          fillSpaces 0 d = d
          fillSpaces n d = fillSpaces (n-1) (d `Concat` Char ' ')