import Test.QuickCheck
import Q
import N 
import C
-- stack test pentru a rula main
{--instance Arbitrary Q where
    arbitrary = do
        Positive n <- arbitrary
        Positive d <- arbitrary
        pure (Q n d)

propAddComm :: Q -> Q -> Bool
propAddComm q1 q2 = (addQ q1 q2) == (addQ q2 q1)

propAddZeroLeft :: Q -> Bool
propAddZeroLeft q = (addQ (Q 0 1) q) == q 
--}
-- Proprietatea 1
prop_natToInt_intToNat x = convertNToInt (convertIntToN x) == x

-- Proprietatea 2
prop_intToNat_natToInt n = convertIntToN (convertNToInt n) == n

-- Definirea generatorului pentru N
instance Arbitrary N where
  arbitrary = sized arbN

arbN :: Int -> Gen N
arbN 0 = return Zero
arbN n = frequency [(1, return Zero), (3, Succ <$> arbN (n-1))]

main :: IO ()
main = do
    {--quickCheck (withMaxSuccess 10000 propAddComm)
    quickCheck propAddZeroLeft--}

    putStrLn "Testing natToInt -> intToNat (da fail)"
    --quickCheck prop_natToInt_intToNat

    putStrLn "Testing intToNat -> natToInt (passed)"
    quickCheck prop_intToNat_natToInt
