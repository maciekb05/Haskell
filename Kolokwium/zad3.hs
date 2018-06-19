kropka f g x = f(g(x))

trzy [] = []
trzy (x:lista) = x:x:x: (trzy lista) 

ilslow = do 
	linia <- getLine
	putStrLn (show $ length (words linia))
