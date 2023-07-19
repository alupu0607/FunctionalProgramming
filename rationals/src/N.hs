module N
  (N(..)
  , addN
  , mulN
  ,subN
  ,divN
  ) where 
data N = Zero | Succ N deriving (Show, Ord)

addN :: N -> N -> N
addN Zero n = n
addN n Zero = n
addN (Succ n1) (Succ n2) = Succ (addN n1 (Succ n2))

mulN :: N -> N -> N
mulN Zero _ = Zero
mulN _ Zero = Zero
mulN n1 (Succ Zero) = n1
mulN n1 (Succ n2) = addN n1 (mulN n1 n2)
  
--subN not complete
subN :: N -> N -> N
subN Zero Zero = Zero
subN n Zero = n
subN Zero n = error "The result is not Nat"
subN (Succ n1) (Succ n2) = subN n1 n2

--divN not complete
divN :: N -> N -> N
divN _ Zero = error "division by zero"
divN Zero _ = Zero
divN n1 n2 = go n1 n2 Zero
  where
    go n1 n2 acc
      | n1 < n2 = acc
      | otherwise = go (n1 `subN` n2) n2 (Succ acc)

instance Eq N where 
  Zero == Zero = True
  (Succ n1) == (Succ n2) = n1 == n2
  _ == _ = False
