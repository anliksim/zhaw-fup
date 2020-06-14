data Tree a
    = Node a (Tree a) (Tree a)
    | Leaf a

sumT :: Num a => Tree a -> a
sumT (Leaf a) = a
sumT (Node a l r) = a + (sumT l) + (sumT r)


t1 = (Node 1 (Leaf 2) (Leaf 3))
s1 = sumT t1 -- 6


sumTR :: Num a => Tree a -> a
sumTR t = sumTR_ (const 0) [t]
    where
      sumTR_ cont [] = cont ()
      sumTR_ cont ((Leaf a):ts) = sumTR_ (\x -> a + (cont x)) ts
      sumTR_ cont ((Node a l r):ts) = sumTR_ (\x -> a + (cont x)) (l:r:ts)

-- better reduction of the problem
-- > only the 'Leaf' case sums up the numbers

sumTR' :: Num a => Tree a -> a
sumTR' t = sumTR_ (const 0) [t]
    where
      sumTR_ cont [] = cont ()
      sumTR_ cont ((Node a l r):ts) = sumTR_ cont ((Leaf a):l:r:ts)
      sumTR_ cont ((Leaf a):ts) = sumTR_ cont' ts
        where
          cont' x = a + (cont x)

