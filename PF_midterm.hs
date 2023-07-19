import Data.Char (ord, isDigit)
--1
sumOfSquare :: Int-> Int
sumOfSquare n = aux n 0 
    where aux n acc 
            | n==0 = acc 
            | otherwise = aux (n-1) (n^2 + acc)

--2
countOccurrences :: Eq a => [a] -> a -> Int 
countOccurrences [] _ = 0 
countOccurrences (x:xs) n 
    | x==n = 1+ countOccurrences xs n 
    | otherwise = countOccurrences xs n

--3
longestIncreasingSubsequence :: [Int] -> Int
longestIncreasingSubsequence [] = 0
longestIncreasingSubsequence (x : xs) = max (1 + (longestIncreasingSubsequence (filter (> x) xs))) (longestIncreasingSubsequence xs)




-- 5 Proiectez un tip de data pt proiectarea expresiilor aritmetice 
data Arithmetic = Var String
                | Val Int
                | Sum Arithmetic Arithmetic
                | Mul Arithmetic Arithmetic
                | Sub Arithmetic Arithmetic
                deriving Show
-- expresia codificata:
-- Sub (Sum (Mul (Var "x") (Val 7)) (Val 10)) (Val -23)

--6 NU ERA CORECT CU CONSTRUCTORI SEPARATI
data Shape  = Circle Float  | Rectangle Float Float | Triangle Float Float Float

area :: Shape -> Float 
area (Circle r) = pi * r * r  
        where pi = 3.14  
area (Rectangle l1 l2 )= l1*l2
area (Triangle l1 l2 l3) = sqrt (p * (p-l1)*(p-l2)*(p-l3))
        where p= (l1+l2+l3)/2

--7 
sumOfEvenSquares :: [Int] -> Int
sumOfEvenSquares [] = 0
sumOfEvenSquares xs = foldl (\acc x -> acc + x * x) 0 (filter even xs)


sumOfEvenSquares2 :: [Int] -> Int
sumOfEvenSquares2 xs = sum (map (\x -> x * x) (filter even xs))


--8 
-- transform lista mea initiala in aia 
-- e higher order pt ca a functia aia anonima
averageGrade :: [(String, [Float])] -> [(String, Float)]
averageGrade = map (\(name, grades) -> (name, average grades))
  where
    average grades = let (sumGrades, count) = foldl (\(sum, count) grade -> (sum + grade, count + 1)) (0, 0) grades
                     in sumGrades / fromIntegral count -- valoarea finala primita

safeHead :: [a] -> Maybe a
--Nothing => nu se afla nimic acolo. 

safeHead = foldr (\x _ -> Just x) Nothing


-- Lab 14
--sirurile de caractere sunt tratate fix ca listele 

--asta compune in sense invers
-- "123" => 321
{-string2Integer :: String -> Integer
string2Integer "" = 0
string2Integer (c:cs) = string2Integer cs * 10 + char2Integer c
  where
    char2Integer :: Char -> Integer
    char2Integer c = (fromIntegral (ord c)) - 48
-}


{-char2Integer :: Char -> Integer
char2Integer c = (fromIntegral (ord c)) - 48

string2Integer :: String -> Integer
string2Integer [] = 0
string2Integer (x:xs) = (char2Integer x) * 10^(length xs) + string2Integer xs
-}


char2Integer :: Char -> Integer
char2Integer c = fromIntegral (ord c) - 48

string2Integer :: String -> Integer
string2Integer = go 0
  where
    go :: Integer -> String -> Integer
    go acc [] = acc
    go acc (x:xs) = go (acc * 10 + char2Integer x) xs

--reverse a number with Accumulator
reverseNumber :: Integer -> Integer
reverseNumber n = reverseHelper n 0
  where
    reverseHelper :: Integer -> Integer -> Integer
    reverseHelper 0 acc = acc
    reverseHelper n acc = reverseHelper (n `div` 10) (acc * 10 + n `mod` 10)


-- countCapitals
countCapitals :: String -> Int 
countCapitals [] = 0
countCapitals (x:xs) = if ord(x) - ord('a') < 0
                        then 1+ countCapitals xs 
                        else countCapitals xs
-- 3
--4 
data Vehicle = Car Brand Model Fabrication |Ship Brand Fabrication |Bicycle Brand Model|Truck Brand Model Weight deriving Show
data Brand  = Name String deriving Show
data Model = ModelName String deriving Show
data Weight = W Int deriving Show
data Fabrication= F Int  deriving Show
-- vehicleBrand (Car (Name "Miu") (ModelName "123") (F 123))
vehicleBrand :: Vehicle -> String 
vehicleBrand (Car (Name name) _ _) = name 
vehicleBrand (Ship (Name name) _) = name 
vehicleBrand (Bicycle (Name name) _) = name 
vehicleBrand (Truck (Name name)_ _ ) = name 

--4 
--5 
--filter !!! Pun la final fix lista peste care vr sa ma uit in continuare
countDigits "" = 0
countDigits (x:xs) = length (filter (\x -> x>='0' && x<='9') (x:xs))

--appFOverList [1,2,3] (\x->[x-1,x+1])
-- chestia asta face lista sa fie mai cuprinzatoare
-- 6 (SA APLIC O FUNCTIE OARECARE .. trebuie sa concatenez rezultatele cu ++)
appFOverList :: [a] -> (a->[a]) -> [a]
appFOverList [] _ = []  
appFOverList (x:xs) f = f x ++  appFOverList xs f