-- file: BogusPattern.hs
data Fruit = Apple | Orange

apple = "apple"

orange = "orange"

whichFruit :: String -> Fruit
whichFruit f = case f of
                 "apple" -> Apple
                 "orange" -> Orange