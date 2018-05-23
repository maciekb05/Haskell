policzISumuj :: (Int -> Int) -> Int -> Int -> Int
policzISumuj f left right = sum $ map f [left..right]
