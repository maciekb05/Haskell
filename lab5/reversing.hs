main = do
    putStrLn "Give me some string to reverse"
    str <- getLine
    if (length str == 0) 
    then return () 
    else do
    	putStrLn $ reverse str
    	main
