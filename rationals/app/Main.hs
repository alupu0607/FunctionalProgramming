module Main (main) where
--stack run pt a rula main
import Q
import N
import C

main :: IO ()
main = do
  putStrLn "Enter two integer values:"
  nStr <- getLine
  mStr <- getLine
  let n = read nStr
      m = read mStr
      natN1 = addN (convertIntToN n) (convertIntToN m)
      intN1 = convertNToInt natN1
      natN2 = mulN (convertIntToN n) (convertIntToN m)
      intN2 = convertNToInt natN2
      natN3 = subN (convertIntToN n) (convertIntToN m)
      intN3 = convertNToInt natN3
      natN4 = divN (convertIntToN n) (convertIntToN m)
      intN4 = convertNToInt natN4
  putStrLn ("The sum of " ++ show n ++ " and " ++ show m ++ " is " ++ show intN1 ++ "." ++ " The multiplication is " ++ show intN2 ++ "." ++ " The substraction is " ++ show intN3 ++ ". The division is " ++ show intN4)

    {--putStrLn "numerator = "
    n <- getLine
    putStrLn "denominator = "
    d <- getLine
    putStrLn $ show $ addQ (Q (read n) (read d)) (Q (read n) (read d))
    --}
    
