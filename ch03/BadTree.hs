-- file: BadTree.hs

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

bad_nodeAreSame (Node a _ _) (Node b _ _)
  | a == b = Just a
bad_nodeAreSame _ _ = Nothing