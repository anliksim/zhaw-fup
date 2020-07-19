-------------------------------------------------------------------------------
-- Teil (a)
-------------------------------------------------------------------------------

-- Gegeben
add2 :: Num a => [a] -> [a] -> [a]
add2 (x:xs) (y:ys) = (x+y):(add2 xs ys)
add2 [] ys = ys
add2 xs [] = xs

-- Implementieren Sie hier eine endrekursive Variante der Funktion
-- add2.
add2TR :: Ord a => [a] -> [a] -> [a]
add2TR xs ys = add2TR_ [] xs ys
    where
        add2TR_ acc [] ys = ys
        add2TR_ acc xs [] = xs
--        add2TR_ acc (x:xs) (y:ys) = add2TR_ (x+y):acc xs ys


-------------------------------------------------------------------------------
-- Teil (b)
-------------------------------------------------------------------------------

-- Gegeben
data Tree a = Leaf a | Node a (Tree a) (Tree a)
    deriving Show


maxElem :: Ord a => Tree a -> a
maxElem (Leaf a) = a
maxElem (Node a l r) = max a $ max ml mr
    where
        ml = maxElem l
        mr = maxElem r 

-- Implementieren Sie hier eine endrekursive Variante der Funktion
-- maxElem.
maxElemTR :: Ord a => Tree a -> a
maxElemTR t = maxElemTR_ (const 0) [t]
    where
      maxElemTR_ cont [] = cont ()
      maxElemTR_ cont ((Node a l r):ts) = maxElemTR_ cont ((Leaf a):l:r:ts)
      maxElemTR_ cont ((Leaf a):ts) = maxElemTR_ cont' ts
        where
          cont' x = 1 + (cont x)