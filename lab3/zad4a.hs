policzISumuj :: (Int -> Int) -> Int -> Int -> Int
policzISumuj func first last = sum $ map func [first..last]
