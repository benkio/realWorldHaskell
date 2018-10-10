safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

unsafeHead :: [a] -> Maybe a
unsafeHead = \(x:_) -> Just x