import Distribution.SPDX (LicenseId(AFL_3_0, BSD_4_Clause, RPL_1_5))
--ULTIMUL EXERCITIU 4.2
--fromendaux :: [a] -> Int ->(a,Int)
--fromendaux [x] index = (x,0)
--fromendaux (x:xs) index = let (x',index') = 
                        -- line-ul asta face binding intre ceea ce se returneaza si ce e in let
--                                fromendaux xs index in
--                          if index' == index then
--                              (x',index)
--                          else
--                               (x,index'+1)

--fromend :: [a] -> Int -> Maybe a
--fromend [] _ = Nothing
--fromend (x:xs) index = let (x', index') =
--                             fromendaux (x:xs) index in
--                        if index == index' then
--                            Just x'
--                        else
--                            Nothing
--                xs     ys
--convolute_aux :: [a] -> [a] -> ([a,a], [a])
--convolute_aux [] ys = ([],ys)
--convolute_aux (x:xs) ys = let (rest,ys') = convolute_aux xs ys in
--                            case ys' of
--                            [] -> (rest, ys)
--                            (y:ys'') ->((x,y):rest,ys'')

--convolute :: [a] -> [a] -> Maybe[(a,a)]
--convolute l1 l2 = let (result, l) = convolute_aux l1 l2 in
--                                    case l 


adder c = \x -> x + c
add2 = adder 2
add3 = adder 3

f :: (Int, Int) -> Int
f (x,y) = x + y
g :: Int -> Int -> Int
g x y = x + y


--ex1
addThree :: (Int, Int, Int) -> Int
addThree (x,y,z) = x + y + z
addThree' :: Int -> Int -> Int -> Int
addThree' x y z = x + y + z

--ex2
process :: (Int -> Int) -> Int -> Int
process f x = f x

process' :: (a -> a) -> a -> a
process' f x = f x

--ex2.1
sum :: (Int -> Int) -> Int -> Int -> Int
sum f a b = foldl (+) 0 (map f [a..b])

--ex2.2
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

--ex2.3 
--(composeList [(+2),(*3)]) 7
composeList :: [b -> b] -> (b -> b)
composeList [] = id
composeList (x:xs) = x . (composeList xs)

--ex2.4 
--sumList [2,3,4]
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

--ex2.5 
--applyToList (+3) [1,2,3]
applyToList :: (a -> b) -> [a] -> [b]
applyToList _ [] = []
applyToList f (x:xs) = f x : applyToList f xs

--ex2.6


