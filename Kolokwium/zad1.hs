{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

data Student = Student {id :: Int, name :: String, dateOfBirth :: String}

data Score = Score {studentId :: Int, course1Score :: Int, course2Score :: Int, course3Score :: Int}

data StudentWithScores = StudentWithScores {ajdi :: Int, names :: String, dateOfBirths :: String, course1Scores :: Int, course2Scores ::  Int, course3Scores :: Int}

instance Show Student where
	show stud = (show (Main.id stud)) ++ "; " ++ name stud ++ "; " ++ dateOfBirth stud
	
instance Show Score where
	show score = (show (studentId score)) ++ "; " ++ (show (course1Score score)) ++ "; " ++ (show (course2Score score)) ++ "; " ++ (show (course3Score score))
	
instance Show StudentWithScores where
	show studWithScores = (show (ajdi studWithScores)) ++ "; " ++ names studWithScores++ "; " ++ dateOfBirths studWithScores++ "; " ++ (show (course1Scores  studWithScores)) ++ "; " ++ (show (course2Scores studWithScores)) ++ "; " ++ (show (course3Scores studWithScores)) 
	
instance Eq Student where
	(==) stu1 stu2 = Main.id stu1 == Main.id stu2

instance Eq Score where
	(==) sc1 sc2 = (course1Score sc1 == course1Score sc2) && (course2Score sc1 == course2Score sc2) && (course3Score sc1 == course3Score sc2)

instance Eq StudentWithScores where
	(==) st1 st2 = ajdi st1 == ajdi st2
	
toStudentWithScores :: Student -> Score -> Maybe StudentWithScores
toStudentWithScores st sc 	| Main.id st == studentId sc = Just (StudentWithScores (studentId sc) (name st) (dateOfBirth st) (course1Score sc) (course2Score sc) (course3Score sc))
							| otherwise = Nothing
							
		
		
			
findStudent :: Int->Score-> Bool
findStudent idst sc = studentId sc == idst
							
findById :: [Score] -> Int -> [Score]
findById scList idStudent  =
	filter (findStudent idStudent) scList
	
-- findBy :: (Score -> Bool) -> [Score] - > [Score]

-- mapBy :: (Score -> b) -> [Score] -> [b]

-- reduceBy :: (a -> Source -> a) -> a -> [Source] -> a



mapToJoin :: Student -> [Score] -> [Maybe StudentWithScores] 
mapToJoin st scList =
	map (toStudentWithScores st) scList
	
joinStep1 :: [Student] -> [Score] -> [(Student, [Score])]
joinStep1 stList scList = [(st,sc) | st<-stList, let sc = (findById scList (Main.id st))]	
	
	

	
joinStep2 :: [(Student, [Score])] -> [[Maybe StudentWithScores]]
joinStep2 [] = []
joinStep2 (x:xs) = (mapToJoin ( fst x ) ( snd x)) : joinStep2 xs


sc2 = Score 13 4 5 6
sc4 = Score 14 4 5 6
sc3 = Score 14 4 5 6
sc1 = Score 11 1 2 3
list = [sc1,sc2,sc3,sc4]
st1 = Student 13 "maciek" "data2"
st2 = Student 14 "aga" "data"
st3 = Student 10 "asfga" "dsdata"
listst = [st1,st2,st3]

class Id a where
	toInt :: (Ord a) => a -> Int
	
instance Id String where
	toInt st = (read st) :: Int
	







	
	
