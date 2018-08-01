a `plus` b = a + b

data a `Pair` b = a `Pair` b
                  deriving (Show)

foo = "hello" `Pair` 2
bar = Pair "hello" 2