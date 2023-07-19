{-# LANGUAGE TemplateHaskell #-}
-- ex1
and1 :: Bool-> Bool -> Bool
and1 False _ = False
and1 _ False = False
and1 _ _ = True

or1 :: Bool -> Bool -> Bool
or1 False False = False
or1 _ _ = True

not1 :: Bool -> Bool
not1 True = False
not1 False = True

nand1 :: Bool -> Bool -> Bool
nand1 True True = True
nand1 _ _ = False

nor1 :: Bool -> Bool -> Bool
nor1 False False = False
nor1 _ _ = True

--not p sau q
implication1 :: Bool -> Bool -> Bool
implication1 False True = True
implication1 False False = True
implication1 True False = False 
implication1 True True = True
-- ex2

hasDivisors :: Integer -> Integer -> Integer -> Bool
hasDivisors n a b | a > b = False
hasDivisors n a b | mod n a == 0 = True
hasDivisors n a b     = hasDivisors n (a+1) (b-1)

isPrime :: Integer -> Bool
isPrime n = not (hasDivisors n 2 (div n 2))

-- ex3

gcd1 :: Integer -> Integer -> Integer
gcd1 a b | b == 0 = a
gcd1 a b         = gcd1 b (mod a b)

-- ex4
-- se poate face acest lucru prin folosirea de acumulatori, altfel renuntandu-se la stiva

-- ex5
fibo :: Integer -> Integer
fibo n | n==0 = 0
fibo n | n==1 = 1
fibo n        = fibo (n-1) + fibo(n-2)

fiboaux :: Integer -> Integer -> Integer -> Integer
fiboaux 0 a _ = a
fiboaux n a b = fiboaux (n-1) b (a+b)
-- a si b sunt doua numere Fibonacci consecutive

fibo' :: Integer -> Integer
fibo' n = fiboaux n 0 1
-- fibo si fibo' sunt echivalente rational:
-- fibo' n = fiboaux n 0 1

--ex7
succ :: Integer -> Integer
succ x = x+1

--ex9 mod si div