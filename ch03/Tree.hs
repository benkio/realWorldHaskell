-- file: ch03/Tree.hs


data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

data Tree' a = Node' a (Maybe (Tree' a))
                       (Maybe (Tree' a))
               deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)