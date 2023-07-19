module C
  (N(..)
  , convertNToInt
  , convertIntToN
  ) where 
import N(N(..))

convertNToInt :: N -> Integer
convertNToInt Zero = 0
convertNToInt (Succ n) = 1 + convertNToInt n

convertIntToN :: Integer -> N
convertIntToN n
  | n <= 0 = Zero
  | otherwise = Succ (convertIntToN (n - 1))
