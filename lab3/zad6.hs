conajmniejn :: (Eq a, Ord b, Num b) => [a] -> b -> [a]
conajmniejn [] n = []
conajmniejn (x:xs) n = 
	if ileWystapien >= (n - 1)
	then x : conajmniejn resztaBezX n
	else conajmniejn resztaBezX n
	where 
		resztaBezX = filter (x/=) xs
		ileWystapien = sum $ map (\y -> if x == y then 1 else 0) xs
