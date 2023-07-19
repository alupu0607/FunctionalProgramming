import Prelude hiding (Right, Left)

data BinaryTree = Nil | Node Int BinaryTree BinaryTree deriving (Show, Eq)

data CrumbBin = Left Int BinaryTree 
              | Right Int BinaryTree
                deriving (Show, Eq)

type ZipperBin = (BinaryTree, [CrumbBin])

goLeft :: ZipperBin -> Maybe ZipperBin
goLeft (Node x l r, crumbs) = Just (l, Left x r : crumbs)
goLeft (Nil, _) = Nothing

goRight :: ZipperBin -> Maybe ZipperBin
goRight (Node x l r, crumbs) = Just (r, Right x l : crumbs)
goRight (Nil, _) = Nothing

goUp :: ZipperBin -> Maybe ZipperBin
goUp (t, Left x r : crumbs) = Just (Node x t r, crumbs)
goUp (t, Right x l : crumbs) = Just (Node x l t, crumbs)
goUp (t, []) = Nothing

change :: Int -> ZipperBin -> Maybe ZipperBin
change v (Node x l r, crumbs) = Just (Node v l r, crumbs)
change v (Nil, _) = Nothing

-- 
--              8
--      2               10
--  3       nil     11       nil
--nil nil         12  nil
--             nil nil
testingBinTree = Node 8 (Node 2 (Node 3 Nil Nil) Nil) (Node 10 (Node 11 (Node 12 Nil Nil) Nil) Nil) 
--

--  Just (testingBinTree, []) >>= goLeft >>= goRight >>= goUp
--  Just (testingBinTree, []) >>= change 2
--  Just (testingBinTree, []) >>= goRight >>= goLeft >>= goRight >>= change 3


data Lista = ListNil 
           | Cons Int Lista
             deriving (Show, Eq)

data CrumbList = Go Int deriving (Show, Eq)

type ZipperList = (Lista, [CrumbList])

goFwd :: ZipperList -> Maybe ZipperList
goFwd (Cons v tl, crumbs) = Just (tl, Go v : crumbs)
goFwd (ListNil, _) = Nothing

change2 :: Int -> ZipperList -> Maybe ZipperList
change2 v' (Cons v tl, crumbs) = Just (Cons v' tl, crumbs)
change2 v' (ListNil, crumbs) = Nothing

goBwd :: ZipperList -> Maybe ZipperList
goBwd (l, Go v : crumbs) = Just (Cons v l, crumbs)
goBwd (_, []) = Nothing
-- goFwd (Cons 10 (Cons 11 (Cons 20 (Cons 121 (Cons 15 ListNil)))), [])
-- goFwd (Cons 2 (Cons 5 ListNil),[]) =>  imi spune 
testingList = Cons 10 (Cons 11 (Cons 20 (Cons 121 (Cons 15 ListNil)))) -- [10,12,20,121,15]
-- Just (testingList, []) >>= chg 121 >>= goFwd 
