import Data.Char

main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    let capsname = map toUpper name
    	revname = reverse name
    putStrLn (capsname ++ " Hello!")
    putStrLn ("Goodbye " ++ revname)  
    
