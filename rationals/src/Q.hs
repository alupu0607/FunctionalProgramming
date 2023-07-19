module Q 
  (Q(..)
  , simpl
  , addQ
  ) where 

-- numerator / denominator 
data Q = Q Int Int 

instance Show Q where
    show (Q n d) = show n ++ "/" ++ show d

simpl :: Q -> Q 
simpl (Q n d) = Q (n `div` c) (d `div` c)
  where c = gcd n d

addQ :: Q -> Q -> Q 
addQ (Q n1 d1) (Q n2 d2) = simpl (Q (nc1 + nc2) d)
    where d = lcm d1 d2 
          nc1 = n1 * (d `div` d1)
          nc2 = n2 * (d `div` d2)

instance Eq Q where 
    (Q n1 d1) == (Q n2 d2) = (n1' == n2') && (d1' == d2')
        where (Q n1' d1') = simpl (Q n1 d1)
              (Q n2' d2') = simpl (Q n2 d2)