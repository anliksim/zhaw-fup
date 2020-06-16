data Tree a
    = Leaf a
    | Node a (Tree a) (Tree a)

t1 = (Node 1 (Leaf 1) (Leaf 1))
t2 = (Node 1 (Node 1 (Leaf 1) (Leaf 1)) (Leaf 1))

depth :: Tree a -> Integer
depth (Leaf _) = 1
depth (Node _ tl tr) = 1 + max (depth tl) (depth tr)

-- Implementieren Sie eine endrekursive Variante der Funktion depth

depthTR :: Tree a -> Integer
depthTR t = depthTR_ (const 0) [t]
    where
      depthTR_ cont [] = cont ()
      depthTR_ cont ((Node _ tl tr):ts) = depthTR_ cont (tl:tr:ts)
      depthTR_ cont ((Leaf _):ts) = depthTR_ (\x -> 1 + (cont x)) ts