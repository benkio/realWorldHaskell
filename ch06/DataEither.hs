data Maybe a = Nothing
             | Just a
             deriving (Show, Eq, Read, Ord)

data Either a b = Left a
                | Right b
                deriving (Show, Eq, Read, Ord)

