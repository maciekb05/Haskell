pierwsze :: [Int] -> [Int]
pierwsze [] = []
pierwsze (x:xs) =
	if pierwsza x
	then x : pierwsze xs
	else pierwsze xs
	where
		pierwsza = \y -> ilePodzielnikow y == 0
		ilePodzielnikow = \y -> sum $ map (czyPodzielna y) [2,3..(y-1)]
		czyPodzielna = \y z -> if y `mod` z == 0 then 1 else 0
