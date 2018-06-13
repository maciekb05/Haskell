import System.IO 
import Data.Char  
 
main = do  
    contents <- readFile "machine.txt"  
    writeFile "capslocked.txt" (fmap toUpper contents)
