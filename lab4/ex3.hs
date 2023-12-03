data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)
    deriving Show

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + max (depthOfBT rt) (depthOfBT lt)

flattenBTPreOrder :: BinTree a -> [a]
flattenBTPreOrder EmptyBT = []
flattenBTPreOrder (NodeBT n lt rt) = flattenBTPreOrder lt ++ [n] ++ flattenBTPreOrder rt

flattenBTInOrder :: BinTree a -> [a]
flattenBTInOrder EmptyBT = []
flattenBTInOrder (NodeBT n lt rt) = [n] ++ flattenBTInOrder lt ++ flattenBTInOrder rt

flattenBTPostOrder :: BinTree a -> [a]
flattenBTPostOrder EmptyBT = []
flattenBTPostOrder (NodeBT n lt rt) = flattenBTPostOrder rt ++ [n] ++ flattenBTPostOrder lt


mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

insertIntoBst :: Ord a => a -> BinTree a -> BinTree a
insertIntoBst x EmptyBT = NodeBT x EmptyBT EmptyBT
insertIntoBst x (NodeBT n lt rt)
  | x == n    = NodeBT n lt rt
  | x < n     = NodeBT n (insertIntoBst x lt) rt
  | otherwise = NodeBT n lt (insertIntoBst x rt)

list2BST :: Ord a => [a] -> BinTree a
list2BST = foldr insertIntoBst EmptyBT