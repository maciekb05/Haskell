import System.IO     
 
main = do     
    contents <- readFile "machine.txt"     
    writeFile "new_machine.txt" contents 
