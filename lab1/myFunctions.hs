myHead (x:_) = x

myLength x = sum [1 | _<-x]  

myTakeNthElement n x = if n == 0 then (head x) else (myTakeNthElement (n-1) (tail x))

myTake n x = if ((myLength x) == n) then x else (myTake n (init x))

myMap f args = [x | y<-args, x<-f(y)]

myPlusPlus :: [a] -> [a] -> [a]
myPlusPlus [] [] = []
myPlusPlus firstList [] = firstList
myPlusPlus [] secondList = secondList
myPlusPlus (h:t) secondList = h : (myPlusPlus t secondList)
