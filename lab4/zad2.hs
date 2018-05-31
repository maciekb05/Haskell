data Osoba = Osoba
	{	imie :: String
	,	nazwisko :: String
	,	pesel :: String
	}
	
instance Eq Osoba where
	(==) x y = pesel x == pesel y

instance Ord Osoba where
	compare x y = nazwisko x `compare` nazwisko y
