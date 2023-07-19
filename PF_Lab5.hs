import qualified GHC.Show as Text
import qualified GHC.Enum as GHC
{- ex01
a) Show
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}

b) class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}

c) class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}

d) 
class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  GHC.Read.readPrec :: Text.ParserCombinators.ReadPrec.ReadPrec a
  GHC.Read.readListPrec :: Text.ParserCombinators.ReadPrec.ReadPrec
                             [a]
  {-# MINIMAL readsPrec | readPrec #-}

e)
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  {-# MINIMAL toEnum, fromEnum #-}

f) class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
 
-}

{- ex02
Lista tipurilor se gaseste in:
Show -> Text.Show
Eq -> GHC.Classes 
Read -> Text.Read 
Enum -> GHC.Enum
Num-> GHC.Num
-}

{-
ex03
-}
data Nat = Zero | Double Nat | DoubleAddOne Nat


{-
ex04 Instante pt Nat ale claselor E1 Ord Integral Num
-}

instance Eq Nat where
  (==) Zero Zero  = True
  (==) (Double n) (Double m) =  n == m 
  (==) (DoubleAddOne n) (DoubleAddOne m) = n == m
  (==)  _ _ = False

instance Ord Nat where
    (<=) Zero n  = True
    (<=) (Double n) (Double m) = n<=m 
    (<=) (DoubleAddOne n) (DoubleAddOne m) = n<=m 
    --(<=) Zero (Double _) = True
    --(<=) Zero (DoubleAddOne _) = True
    (<=) (Double _) (DoubleAddOne _) = True
    (<=) _ _ = False

{-Creati un tip Complex pt nr complexe ... 
Complex Int, Complex Nat. Instantati clasa Num-}
data Complex a = Im a a deriving (Show, Eq, Ord)

{-instance Num (Complex a) where
    (+) (Im a1 b1) (Im a2 b2) = Im (a1 + a2) (b1+b2)
    (-) (Im a b) = Im (-a)(-b)
    (*) (Im a1 b1) (Im a2 b2) = Im(a1*a2-b1*b2)(a1*a2+b1*b2)
    fromInteger a = Im a 0
    signum (Im a b)| a ==0 && b==0 =0
                   | a>0 && b>0 =1
                   | a<0 && b<0 =0
                   | otherwise = -1-}
