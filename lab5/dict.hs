import System.IO 
import Data.Char  
 
main = do  
    contents <- readFile "machine.txt"
    dictionary <- readFile "/usr/share/dict/words"
    writeFile "notInDictionary.txt" (findWords contents dictionary)
  
findWords contents dictionary = unwords $ filter ( \x -> not $ elem x $ words $ fmap toLower dictionary ) ( fmap (filter (isCharFromWord)) $ words $ fmap toLower contents )

isCharFromWord char = (isLetter char) || (char == '\'')
