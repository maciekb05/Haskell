{-# LANGUAGE FlexibleInstances #-}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

insert :: (Ord a) => a -> Tree a -> Tree a
insert a Empty = Node a Empty Empty
insert a (Node x left right)
	| a == x = Node x left right
	| a < x = Node x (insert a left) right
	| a > x = Node x left (insert a right)
	
empty :: Tree a -> Bool
empty Empty = True
empty tree = False

search :: (Ord a) => a -> Tree a -> Bool
search a Empty = False
search a (Node x left right)
	| a == x = True
	| a < x = search a left
	| a > x = search a right

toString :: (Show a) => Tree a -> String
toString Empty = ""
toString (Node x left right) = show x ++ "(" ++ toString left ++ "," ++ toString right ++ ")"

leaves :: (Eq a) => Tree a -> [a]
leaves Empty = []
leaves (Node x left right) = if left == Empty && right == Empty then [x] else leaves left ++ leaves right

nnodes :: Tree a -> Int
nnodes Empty = 0
nnodes (Node x left right) = 1 + nnodes left + nnodes right 

nsum :: Tree Int -> Int
nsum Empty = 0
nsum (Node x left right) = x + nsum left + nsum right

remove :: (Ord a) => a -> Tree a -> Tree a
remove a Empty = Empty -- Usuwanie z pustego drzewa
remove a (Node x left right) -- Poszukiwanie rekurencyjne Node'a do usuniecia
	| a < x = Node x (remove a left) right
	| a > x = Node x left (remove a right)
remove _ (Node _ Empty Empty) = Empty -- Jesli znaleziony nie ma dzieci
remove _ (Node _ left Empty) = left -- Jesli znaleziony ma jedno poddrzewo lewe
remove _ (Node _ Empty right) = right -- Jesli znaleziony ma jedno poddrzewo prawe
remove _ (Node _ left right) = Node newRoot left rightWithoutNewRoot -- Jesli znaleziony ma dwa poddrzewa: zrob jedno poddrzewo z dwoch znajdujac najmniejszy element prawego (nowy root) i laczac z nim lewe i prawe (bez nowego roota)
  where newRoot = minElement right -- najmniejszy element prawego
        rightWithoutNewRoot = remove newRoot right -- new right subtree with min key removed
        -- minElement znajduje najmniejszy element drzewa
        minElement (Node element Empty _) = element
        minElement (Node _ left _) = minElement left

myTree :: Tree Int
myTree = Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty)


