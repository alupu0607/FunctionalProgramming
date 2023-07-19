type Id = String

data Term = Var Id
    | App Term Term
    |  Lambda Id Term deriving (Show, Eq)

-- ex1 λx.λy.x
--Lambda "x" (Lambda "y" (Var "x"))

--ex2
{-
1. x[x/y] = y;  -- subst "x" (Var "y") (Var "x")
2. x[y/z] = x;  -- subst "x" (Var "y") (Var "z")
3. (x y)[y/z] = x z;  -- subst "y" (Var "z") (App (Var "x") (Var "y"))
4. (y x)[y/z] = z x;  -- subst "y" (Var "z") (App (Var "y") (Var "x"))
5. (λx.(y x))[x/(λz.z)] = λx.(y x);  -- subst "x" (Lambda "z" (Var "z")) (Lambda "x" (App (Var "y") (Var "x")))
6. (λy.(y x))[x/(λz.z)] = λy.(y (λz.z))  -- subst "x" (Lambda "z" (Var "z")) (Lambda "y" (App (Var "y") (Var "x")))
-}
subst :: Id -> Term -> Term -> Term
subst id term (Var id') 
  | id == id' = term
  | otherwise = Var id'
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)
subst id term (Lambda id' term') 
  | id == id' = Lambda id' term'
  | otherwise = Lambda id' (subst id term term')

--ex3
--remove "x" ["x", "y", "z"]

remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl) | id == hd = remove id tl
                  | otherwise = hd : remove id tl

--ex4
--free (Lambda "x" (Lambda "y" (App (Var "x") (Var "y"))))
free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = remove id (free term)
    where remove id lst = [x | x <- lst, x /= id]

--ex5
--vars (Lambda "x" (Lambda "y" (App (Lambda "x" (Lambda "y" (Var "x"))) (Var "y"))))
--vars (Lambda "x" (Lambda "y"  (Var "y"))))
vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = vars term1 ++ vars term2
vars (Lambda id term) = id : vars term ++ remove id (free term)
  where remove id lst = [x | x <- lst, x /= id]
--ex6
fresh' :: [Id] -> Int -> Id
fresh' ids index = if ("n" ++ (show index)) `elem` ids
                   then fresh' ids (index + 1)
                   else "n" ++ (show index)

fresh :: [Id] -> Id
fresh ids = fresh' ids 1

--ex7

{-casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') avoid
  | id == id' = term
  | otherwise = Var id'
casubst id term (App term1 term2) avoid = 
  App (casubst id term term1 avoid) (casubst id term term2 avoid)
casubst id term (Lambda id' term') avoid 
  | id == id' = Lambda id' term'
  | id' `elem` freeVars = 
    let id'' = fresh avoid
        term'' = casubst id term (subst id' (Var id'') term') (id'' : avoid)
    in Lambda id'' term''
  | otherwise = Lambda id' (casubst id term term' avoid)
  where freeVars = free term
        freshIds = [id'' | i <- [1..], let id'' = fresh avoid, id'' `notElem` freeVars]
        fresh = head freshIds
        -}