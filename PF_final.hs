import DynFlags (xFlags, xopt_set)
-- 'a' /= 'b' inseamna ca !=
-- in Haskell functiile se scriu cu LITERE MICI
-- functions: name arg1 arg2 .. argn

-- functiile pot fi scrise si fara antet
-- in_range min max x =
--    x>= min && x<=max
-- !!! Ceea ce returneaza o functie este scris dupa =

-- in_range 0 5 3
-- Integer, Bool, Float


--antet (functii => litere mici)
in_range :: Integer-> Integer-> Integer-> Bool
in_range min max x  = x>=min && x<=max

-- let bindings (binds something to a result)
in_range2 min max x = 
  let
    in_lower_bound = min <= x
    in_upper_bound = max >= x
  in
    in_lower_bound && in_upper_bound

in_range3 min max x =
    if ilb then iub else False
    where 
        ilb = min<=x
        iub = max >=x

--functions can be written in an infix syntax

-- add a b = a + b
-- add 10 20
-- 10 'add' 200 // varianta INFIXATA!


fac n =
  if n <= 1 then
    1
  else
    n * fac (n - 1)


-- guards (atunci cand am if-then-else-uri multiple)
-- guards accepta booleans ca conditii si la rezultat punem un egal
fac2 n 
    | n <= 1 = 1
    |otherwise = n * fac (n-1)

is_zero 0 = True
is_zero _ = False



--Functie cu liste
asc :: Int -> Int -> [Int]
asc n m
  | n > m     = []
  | n == m    = [m]
  | otherwise = n : asc (n + 1) m

-- head, tail, length, init (gives you a list with the last element removed)
-- null (determines if it is empty or not)
-- and, or 
-- [2* x | x<- [1,2,3], x>1] => [4,6] 


--LIST PATTERNS (Pattern matching cu liste)

sum1 :: [Int] -> Int
sum1 []     = 0
sum1 (x:xs) = x + sum1 xs


--se duce recursiv la celelalte elemente
evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) -- aici nu am niciun =
    | mod x 2 == 0 = x : evens xs
    | otherwise = evens xs


--TUPLE
fst1 :: (a,b) -> a 
fst1 (a, _ ) =a 

snd1 :: (a,b) -> b 
snd1 (_,b) = b


--PATTERN MATCHING PE LIST TUPLES... Acolo unde am tuple va face suma lor
-- addTuples [(1,2), (2,3), (100,100)]
-- [3,5,200]
addTuples :: [(Int,Int)] -> [Int]
addTuples xs = [x+y | (x,y) <- xs]



-- !!!!!!!!!!!!!!!!!!
-- create a function tht returns True if an 
-- elem is in a given list and return False otherwise
elem1 :: (Eq a) => a -> [a] -> Bool
elem1 _ []     = False
elem1 e (x:xs) = (e == x) || (elem1 e xs)
 

-- removes duplicates from a list
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs)
    | elem1 x xs = nub xs  -- remove dulpicates - efectiv sar aia si nu mai pun : 
    | otherwise = x : nub xs
 

-- !!!!!!!!!!!!!!
-- representative function
isAsc :: [Int] -> Bool
isAsc [] = True 
isAsc [x] = True 
isAsc (x:y:xs) = 
    (x <= y) && isAsc (y:xs) -- urmatoarele de dupa x



-- EXERCITIU CU GRAFURI
-- [(1,2), (2,3), (3,2), (4,3), (4,5)]
hasPath :: [(Int,Int)] -> Int -> Int -> Bool 
hasPath [] x y = x == y  -- no edges to look at
hasPath xs x y 
    | x == y = True  -- am edges, dar start = end
    | otherwise = 
        let xs' = [(n,m)| (n,m) <-xs, n/= x] in 
        or  [hasPath xs' m y | (n,m) <- xs, n==x]

-- Higher Order Functions -- da call la alte functii 
-- app add1 1 => 2
app :: (a->b) -> a -> b 
app f x = f x 

add1 :: Int -> Int 
add1 x = x+1
-- echvalent:
-- app (\x -> x+1) 1



-- functii anonime => nu vr sa le fac cu nume
-- add1 = (\x -> x+1)
-- (\x y z -> x + y + z) (functie anonima  cu mai multi parametri)
-- (\x y z -> x +  y + z)

-- MAP changes the list 
-- efectiv le folosesc ca atare

-- MAP TRANSFORMS THE LIST
-- map (\x -> x+1) [1,2,3,4,5]
-- map (\(x,y) -> x+y)[(1,2),(2,3),(3,4)]

--FILTER functioneaza precum un IF 
-- filter (\x -> x>2)[1,2,3,4,5]
-- filter (\(x,y) -> x/=y) [(1,2),(2,2)]   => [(1,2)] -- asa ma refer la lista asta ca fiind perechi

 

 --CURRYING 
 -- add :: Int -> Int -> Int 
 -- add x y = x + y 
 -- add x = (\y -> x + y)
 -- add = (\x -> (\y-> x + y))

 -- map 
doubleList :: [Int] -> [Int]
doubleList = map (\x -> 2 * x)


--(f.g) equivalent to (\x -> f (g x))
--descSort = reverse.sort COMPOZITIE
--(\x -> reverse(sort x))
--descSort x = reverse(sort x)

-- map2D (\x -> x * 2) [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
map2D :: (a->b) -> [[a]] -> [[b]] 
map2D = map.map


-- $ applies the function
-- map e doar oe liste 
f xs = map (\x -> x+1)(filter(\x -> x>1) xs)
-- echivalent cu : 
-- f xs = map (\x -> x+1) $ filter (\x -> x>1) xs

-- folding - ACUMULATORU ela dreapta 
-- foldr (fold RIGHT)
-- takes one function as its first argument 
-- foldr (+) 0 [1,2,..n]
-- sum = foldr (+) 0 
-- and = foldr (&&) True 
-- or = foldr (||) False 

-- I want to build a function count which
-- counts how many times an accumulator is in a list
-- foldr (\elem acc -> <term>) <start_acc> <list>




--defining datatypes 
-- data Name = Constructor 1 <args> | Constructor2 <args>
data Color = Red| Orange | Yellow | Green 
data PeaNum = Succ PeaNum | Zero deriving Show  -- NATURAL NUMBERS!!! 
data Calculation = Add Int Int | Sub Int Int | Mul Int Int | Div Int Int

calc :: Calculation -> Int 
calc (Add x y) = x + y 
calc (Sub x y) = x - y 
calc (Mul x y) = x * y 
calc (Div x y) = div x y -- impartirea in Haskell

-- polymorphic type => inseamna ca pun a 
data Tree a = Leaf | Node (Tree a) a (Tree a)
tree :: Tree Int  
tree = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf ) 4 Leaf) 

incr :: PeaNum -> PeaNum
incr = Succ 

--decr :: PeaNum -> PeaNum
--decr (Succ n) = n

add :: PeaNum -> PeaNum -> PeaNum  
add Zero n = n 
add (Succ m) n = Succ $ add m n 

sum2 :: [PeaNum] -> PeaNum 
sum2 [] = Zero 
sum2 (x:xs) = add x $ sum2 xs 


-- !!!!!!!!!!!!! Trb pus si tipul ala Num a
-- create a function that reverses a list 
rev :: Num a => [a] -> [a]
rev  [] = [] 
rev (x:xs) = rev xs ++ [2*x] -- reverse the tail and append the head

rev2 :: [a] -> [a]
rev2= foldl (\acc x -> x : acc) []


--create a function prefixes that returns all the prefixes of a given list 
prefixes :: [a]-> [[a]]
-- prefixes [1,2,3] => [[1],[1,2],[1,2,3]]
prefixes = foldr (\x acc -> [x] : (map ((:) x) acc)) []


data Trie a = Leaf1 a | Node1 a [Trie a]
-- a trie doesnt have to have a subtree
-- it can replace hashmaps
--PREORDER. In ordinea elementelor => foldl
-- in ordine inversa -> foldr

foldtrie :: (b->a->b)->b->Trie a -> b 
foldtrie f acc (Leaf1 x) = f acc x 
foldtrie f acc (Node1 x xs) = foldl f' (f acc x) xs 
 where 
    f' acc t = foldtrie f acc t

-- Records 
 
data Person = Person { name :: String, 
                        age :: Int }

greet :: Person -> [Char]
greet person = "Hi " ++ name person


-- Typeclasses (polymorphism)
-- Num 
-- +,-,*, negate, abs, signum
-- printing a type => has an instance of the Type class 

-- Ord (we have ordering <, >, <=, >=, max, min)
-- everything that can be Ordered can also be Eq'ed 

data Temperature = C Float | F Float  

--TYPECLASSES => creez instances
instance Eq Temperature where 
    (==) (C n) (C m) = n==m 
    (==) (F n) (F m) = n==m 
    (==) (C c)(F f)= (1.8*c+32) == f
    (==) (F f)(C c)=(1.8*c+32) == f

-- Maybe (Nothing, just, -> Maybe)
safeDiv :: Integral a => a -> a -> Maybe a 
safeDiv a b =
    if b==0 then Nothing else Just $ div a b

-- isNothing, isJust -> filtering on Maybe's
 
-- IO 
-- ca sa scriu : putStrLn
hw=putStrLn "Hello World"
hw :: IO ()

-- ca sa citesc : getLine 
-- getLine :: IO String
greet2 :: IO()
greet2 = do
    putStrLn "What is ur name?"
    name<-getLine
    putStrLn ("Hello" ++ name ++ ".")

--subprogram care afiseaza pe o linie toate numerele intr-un anumit interval 

count2 :: Int -> Int -> IO()
count2 n m = do -- acolo se pun parametri
    putStrLn (show n) -- asta il arata pe n-ul citit de la tastatura
    if n < m then 
     count2 (n+1) m 
    else 
        return ()




-------------------------------
-------------------------------
--------------------------------

-- suma numerelor naturale pana la acel numar 
sum3:: Int -> Int 
sum3 x = -- se pune si argumentul acolo 
    if x<=0 then 
        0 
    else 
        x+sum3(x-1) 

-- fct rceursiva care returneaza elem de pe poz data din sirul lui Fibonacci 

-- functie pt cmmdc

--functie scrisa cu tot cu acumulator
-- cand scriem ceva recursiv cu acumulator => il punem by default 1
-- tail recursive = ACUMULATOR 
-- auxiliary functions  = > ACUMULATOR
fac3 n = aux n 1
    where 
        aux n acc 
            | n <= 1 = acc 
            | otherwise = aux (n-1) (n*acc)

-- suma intr o lista cu acumulator
sumAcc :: [Int] -> Int
sumAcc xs = sumHelper xs 0
  where
    sumHelper [] acc = acc
    sumHelper (x:xs) acc = sumHelper xs (acc + x)

-- suma tuturor elementelor dintr=o lista: 
sum4 :: [Int] -> Int
sum4 [] = 0  -- Base case: empty list, sum is 0
sum4 (x:xs) = x + sum4 xs  -- Recursive case: add current element to the sum of the remaining list


data Day1 = Mon | Tue | Wed | Thu| Fri| Sat | Sun 

--nextDay :: Day1 -> Day1 -- trb sa pun aici derving Show daca imi fac eu tipurile mele
-- cand definesc tipurile mele, daca vr sa zic ca pot fi comparabile, pun acolo deriving (Show, Eq)

data Rezultat = Nimic | Un Int
--Constructorul Nimic are 0 parametri.
--Constructorul Un are 1 paramatru (ˆıntreg mas, in ̆a).

-- un nou tip pereche care are doi parametri
data Pereche = P Int Int deriving (Show, Eq)
prima :: Pereche -> Int
prima (P x y) = x

adoua :: Pereche -> Int
adoua (P x y) = y

-- Tipuri de date RECURSIVE
-- constructorul Vida are 0 argumente 
-- Cons are 2 arg 
-- Cons 5 (Cons 4 (Cons 3 Vida))
data Lista = Vida | Cons Int Lista deriving (Show, Eq)

--lungime de orice ar fi acolo si restul, tail 
lungime :: Lista -> Int
lungime Vida = 0
lungime (Cons _ tail) = 1 + lungime tail


-- Tipuri de date parametrice  (ca sa mearga pt orice input am da)
data List a = Emp | Con a (List a) deriving (Show, Eq)

-- ca sa mearga pt orice punem asa la modul general 
convert :: [a] -> List a 
convert [] = Emp 
convert (x:xs) = Con x (convert xs)





-- data Somedata = left Int | Right String 
-- data Either a b = Left a | Right b
-- type SomeData = Either Int String

-- EITHER!!! 
data Pozitie = Prima | Adoua deriving Show -- obligatoriu ii dau deriving Show

impartire''' :: Integer -> Integer -> Maybe Integer
impartire''' x y = if y == 0 then
                     Nothing
                   else
                     Just (x `div` y)

                 
impartireInLant' :: Integer -> Integer -> Integer -> Either Pozitie Integer
impartireInLant' x y z = case impartire''' x y of
                           Nothing -> Left Prima
                           Just r -> case impartire''' r z of
                                       Nothing -> Left Adoua
                                       Just r' -> Right r'

-- deci cand combinam tipurile asa ambele trebuie sa aib aacolo deriving (Show, Eq)     
data MobileDevice = Smartphone Culori| Laptop Culori| Tablet Culori deriving (Show, Eq)
data Culori = Rosu| Galben | Albastru deriving (Show, Eq)


showColor1:: MobileDevice -> Culori
showColor1 (Smartphone c) = c
showColor1 (Laptop c) = c
showColor1 (Tablet c) = c

--arbori binari !!!!! ffff important 
data Arb = Frunza | Nod Integer Arb Arb deriving (Show, Eq)
isBST :: Arb -> Bool
isBST Frunza = True
isBST (Nod val stanga dreapta) =
    let 
        subarboreStangaOK = isSubarboreBST stanga Nothing (Just val)
        subarboreDreaptaOK= isSubarboreBST dreapta (Just val) Nothing 
    in 
        subarboreDreaptaOK && subarboreStangaOK
-- la modul general 

-- pastram valoarea curenta, lowerBound-ul si upperBoundul
isSubarboreBST :: Arb -> Maybe Integer -> Maybe Integer -> Bool
isSubarboreBST Frunza _ _ = True
isSubarboreBST (Nod val stanga dreapta) lowerBound upperBound =
  let
    subarboreStangaOK = case upperBound of -- cu caseUpperBound analizez ce e acolo... daca era cu Either era Left/ Right
      Just ub -> val < ub
      Nothing -> True -- daca nu e nimc in subarbore, default True 
    subarboreDreaptaOK = case lowerBound of
      Just lb -> val > lb
      Nothing -> True
  in
    subarboreStangaOK && subarboreDreaptaOK && isSubarboreBST stanga lowerBound (Just val) && isSubarboreBST dreapta (Just val) upperBound
    -- caut atat in stanga cat si si dreapta cat si recursiv in stg si dreapta 

search :: Arb -> Integer -> Bool
search Frunza _ = False  -- Value not found in the leaf node
search (Nod val stanga dreapta) x
  | x == val = True  -- Value found in the current node
  | x < val = search stanga x  -- Value may be in the left subtree
  | otherwise = search dreapta x  -- Value may be in the right subtree

insert :: Arb -> Integer -> Arb
insert Frunza x = Nod x Frunza Frunza  -- Inserting into an empty tree creates a new leaf node
insert (Nod val stanga dreapta) x
  | x < val = Nod val (insert stanga x) dreapta  -- Insert in the left subtree if the value is smaller
  | x > val = Nod val stanga (insert dreapta x)  -- Insert in the right subtree if the value is larger
  | otherwise = Nod val stanga dreapta  -- Value already exists in the tree, no need to insert again


--remove :: Arb -> Integer -> Arb
--remove Frunza _ = Frunza  -- Removing from an empty tree returns an empty tree
--remove (Nod val stanga dreapta) x
--  | x < val = Nod val (remove stanga x) dreapta  -- Value is smaller, remove from the left subtree
--  | x > val = Nod val stanga (remove dreapta x)  -- Value is larger, remove from the right subtree
--  | otherwise = removeNode (Nod val stanga dreapta)  -- Value found, remove the node
-- .....

preOrder :: Arb -> [Integer]
preOrder Frunza = []  -- Empty tree, return an empty list
preOrder (Nod val stanga dreapta) = [val] ++ preOrder stanga ++ preOrder dreapta

inOrder :: Arb -> [Integer]
inOrder Frunza = []  -- Empty tree, return an empty list
inOrder (Nod val stanga dreapta) = inOrder stanga ++ [val] ++ inOrder dreapta

postOrder :: Arb -> [Integer]
postOrder Frunza = []  -- Empty tree, return an empty list
postOrder (Nod val stanga dreapta) = postOrder stanga ++ postOrder dreapta ++ [val]




--Functii de ordin superior

--Functia asta adauga 2 la ce primeste
adder1 c = \x -> x + c
--nu e necesar sa declar altcv acolo
add2 = adder1 2



summing :: (Int -> Int) -> Int -> Int -> Int
summing f x y
  | x > y     = 0
  | otherwise = (+) (f x)  (summing f (x + 1) y)

-- return compunerea a doua functii

-- ex. 2.2
composition :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> Int -> Int -> Int
composition f g x y = f (g x y) (g x y)


--
composition2 :: [Int->Int] -> Int -> Int 
composition2 [] = id
composition2 (x : xs) = x . (composition2 xs) -- compunerea a doua functii in Haskell : .  

-- suma delemente lor dintr o lista 
suma :: [Int] -> Int 
suma [] =0 
suma (x:xs) = x + suma xs

transform [] = 0
transform (x:xs) = 2*x + (transform xs)

--transform2 xs = foldl  (\acc x -> acc + 2*x) 0 xs
--ordinea operatiilor intr o lista => de la stg la dreapta, deci folosesc FOLDL
transform2 :: [Int] -> Int
transform2 = foldl (\acc x -> acc + 2*x) 0. filter (\x -> x `mod` 2 == 0)

count e =
    foldr (\x acc -> if e == x then acc + 1 else acc ) 0


filtering :: (a -> Bool) -> [a] -> [a]
filtering f [] = []
filtering f (x : xs) = if f x
                        then x : (filtering f xs) -- o pastrez
                       else
                        (filtering f xs)

-- clase de tipuri =>
-- clasa Eq : minimal ==
-- clasa Ord : minimal <=
-- bounded: minBound 

data Dow = Mon1 | Tue1 | Wed1 

-- Show este clasa tipurilor
-- ale caror valori pot fi
-- transformate intr-un sir
-- de caractere

-- oarecum: clasele de tipuri seamana cu interfetele

instance Show Dow where
  show Mon1 = "Luni"
  show Tue1 = "Marti"
  show Wed1 = "Miercuri"

instance Eq Dow where
  Mon1 == Mon1 = True
  Tue1 == Tue1 = True
  Wed1 == Wed1 = True
  _    == _    = False




data Nat = Zero1 | Succ1 Nat deriving Ord

n0 = Zero1
n1 = Succ1 n0
n2 = Succ1 n1
n3 = Succ1 n2
n4 = Succ1 n3

add0 :: Nat -> Nat -> Nat
add0 Zero1 x = x
add0 (Succ1 y) x = Succ1 (add0 y x)

convert1 :: Nat -> Int
convert1 Zero1 = 0
convert1 (Succ1 x) = 1 + (convert1 x) -- adun 1 de fiecare data


instance Show Nat where
  show x = show (convert1 x)

instance Eq Nat where
  (==) Zero1 Zero1 = True
  (==) Zero1 _ = False
  (==) (Succ1 _) Zero1 = False
  (==) (Succ1 x) (Succ1 y) = (==) x y
-- Ord: implementez doar <=


--constrangeri de tipuri

sort :: Ord a => [a] -> [a]
sort [] = []
sort (hd:tl) = (sort (filter (<=hd) tl)) ++ [hd] ++ (sort (filter (>hd) tl))


data Exc = DivByZero | NotDefined deriving Show

impartire :: Int -> Int -> Either Exc Int
impartire _ 0 = Left DivByZero
impartire x y = Right (x `div` y)

cmmdc :: Int -> Int -> Either Exc Int
cmmdc 0 0 = Left NotDefined
cmmdc x 0 = Right x
cmmdc x y = if x <= y then
              cmmdc x (y - x)
            else
              cmmdc y (x - y)

x = fmap (+7) (impartire 10 2)
x' = case (impartire 10 2) of
      Left err -> Left err
      Right r -> Right ((+7) r)

-- clasa functor => definesc fmap
--instance Functor List where
--  fmap _ Nil = Nil
--  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


-- intoarce minimul unei liste, calculand primul elem din lista sortata
sort2 [] = []
sort2 (x:xs) = (sort2 (filter(<=x) xs)) ++ [x] ++  (sort2 (filter(>x) xs))

showMin = sort2 [3,2,9] !! 0

-- ultimul element din lista sortata
showLast [] = error "Empty list"
showLast xs = xs' !! (i-1)
    where 
        i = length xs
        xs'= sort2 xs

-- infinite lists: ones = 1: ones 
-- take => ia primele n elemente 

ones = 1 : ones 
result3 = take 5 (map (*2) ones )

nat = asc 1 
    where asc n = n: (asc $ n+1)

odds = filter(\x -> mod x 2 == 0) nat

-- zipWith returneaza o noua LISTA - combina doua liste impreuna
fibs = 0: 1: zipWith (+) fibs (tail fibs)

-- True False, in care nr prime sa aiba asociata valoarea de True 

hasAtleast1Divisor :: Int -> [Int] -> Bool
hasAtleast1Divisor a [] = True
hasAtleast1Divisor a (x : xs) = if (a `mod` x == 0) 
                                    then 
                                        False
                                    else 
                                        (hasAtleast1Divisor a xs)

isPrime :: Int -> Bool
isPrime a = a > 1 && hasAtleast1Divisor a [2..(a `div` 2)]

isPrimeListStartingWith :: Int -> [Bool]
isPrimeListStartingWith x = (isPrime x) : (isPrimeListStartingWith (x + 1))

isPrimeList = isPrimeListStartingWith 0

-- list of prime numbers 
primes :: [Int]
primes = sieve [2..]
  where
        sieve [] = []
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0] -- o sa compare cu toti p-ii dinainte 

-- monada IO 
