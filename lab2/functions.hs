fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

sign :: Double -> Double
sign x | x > 0  = 1
       | x == 0 = 0
       | otherwise = -1
       
sum2a :: (Int, Int) -> Int
sum2a (m, n) = m + n

sum2b :: [Int] -> Int
sum2b (m:n:_) = m + n

sum2c :: Int -> Int -> Int
sum2c m n = m + n

myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

bacteries :: Int -> (Int, Int)
bacteries 0 = (1,1)
bacteries sec = 
	let a = fst (bacteries (sec - 1));
		b = snd (bacteries (sec - 1))
	in (b, 2*a+b)
	
bacteriesA :: Int -> (Int, Int)
bacteriesA 0 = (2,1)
bacteriesA sec = 
	let a = fst (bacteriesA (sec - 1));
		b = snd (bacteriesA (sec - 1))
	in (b, 2*a+b)
	
sumOfDigits :: Int -> Int
sumOfDigits 0 = 0
sumOfDigits x = (x `mod` 10) + (sumOfDigits (x `div` 10))

superNumber :: Int -> Int
superNumber num  
	| num < 10 = num
	| otherwise = superNumber (sumOfDigits num)

usunDuplikaty :: Eq a => [a] -> [a]
usunDuplikaty [] = []
usunDuplikaty (x:xs)
	| x `elem` xs = x : usunDuplikaty (filter (x/=) xs)
	| otherwise = x : usunDuplikaty xs

ileWystapien :: (Num b, Eq a) => a -> [a] -> b
ileWystapien _ [] = 0
ileWystapien y (x:xs) = if y == x then 1 + ileWystapien y xs else ileWystapien y xs

conajmniej :: (Eq a, Ord b, Num b) => [a] -> b -> [a]
conajmniej [] _ = []
conajmniej (x:xs) n
	| ((ileWystapien x xs) + 1) >= n = x : reszta
	| otherwise = reszta
	where reszta = conajmniej (filter (x/=) xs) n

