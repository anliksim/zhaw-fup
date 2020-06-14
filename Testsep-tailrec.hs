data Tree a
    = Node a (Tree a) (Tree a)
    | Leaf a

sumT :: Num a => Tree a -> a
sumT (Leaf a) = a
sumT (Node a l r) = a + (sumT l) + (sumT r)

