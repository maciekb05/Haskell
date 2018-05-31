funkcja :: (a -> Bool) -> [a] -> Maybe a
funkcja f [] = Nothing
funkcja f (x:xs) = if f x == True then Just x else funkcja f xs
