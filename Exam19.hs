-- Aufgabe 1

-- (a)

-- Memory Pointer kann auf bisher erstest Element zeigen anstatt den Pointer des bisher letzen Elementes anzupassen (? wild guess)


-- (b)

mkLists :: Integer -> [[Integer]]
mkLists 0 = []
mkLists n = mkLists(n-1) ++ [[1..n]]

mkl1 = mkLists 1
mkl2 = mkLists 2
mkl3 = mkLists 3

-- (c)

-- code ref?

-- (d)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

-- w/o using last
safeLast' :: [a] -> Maybe a
safeLast' [] = Nothing
safeLast' (_:xs) | length xs >= 1 = safeLast xs
safeLast' (x:_) = Just x

-- (e)

last1 (x:y:xs) = last1 (y:xs)
last1 [x] = x

last2 = head . reverse

-- Beweis: last1 xs == last2 xs

-- xs => (x,x_1,x_2,...,x_n) mit n = length xs

-- last1 (x:y:xs)
--      = (y:xs)
--      = ...
--      = x
--    => (x,x_1,(x_2,...x_n))
--    => (x_1,x_2,(x_3,...,x_n))
--    => ...
--    => x_n

-- last2 xs = head . reverse xs
--          = (head (reverse xs))
--    => head (reverse (x,x_1,x_2,...,x_n))
--    => head (x_n,...,x_2,x_1,x)
--    => x_n


-- Aufgabe 2

-- (a)

-- (b)

-- (i)
-- (4 * 3) + (2 + 1)

-- (ii)

-- (iii)


-- Aufgabe 3


-- Aufgabe 4

-- (a)

-- (b)

-- (i)
--
-- (Lx.(x x) y)
--  ^ beta-Reduktion (normal order)
-- (y y)
--
-- > beta-Normalform


-- (ii)
--
-- (y Lx.(x x))
--
-- > beta-Normalform?


-- (iii)
--
-- (Lx.(x x x) Lx.(x x x))
--
-- > nicht beta-Normalform


-- (c)

-- t = (((Lxyz.((x y) (xz)) Lx.a) b) c)

-- (i)
-- a,b,c

-- (ii)
--
-- (((Lxyz.((x y) (x z)) Lx.a) b) c)
-- ((Lyz.((Lx.a y) (Lx.a z)) b) c)
-- ((Lx.a b) (Lx.a c))
-- (a (Lx.a c))
-- (a a)


