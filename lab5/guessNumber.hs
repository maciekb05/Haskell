import System.Random

smallest = 1 :: Integer
biggest = 10 :: Integer
maxTries = 3 :: Integer

main :: IO ()
main = do
  secret <- randomRIO (smallest, biggest) :: IO Integer
  putStrLn "Find the number between 1 and 10."
  loop secret 0 
  
loop :: Integer -> Integer -> IO ()
loop secret 3 = do
      putStrLn "You failed!"
      putStr "It was: "
      print secret
loop secret tries = do
  guess <- readLn :: IO Integer
  case compare guess secret of
    LT -> do
      putStrLn "Too small!"
      loop secret (tries + 1)
    GT -> do
      putStrLn "Too big!"
      loop secret (tries + 1)
    EQ -> do
      putStrLn "You found the number!"
