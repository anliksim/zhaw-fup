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

-- Normal order reduction:
--
-- (Lx.(x y)Lz.z)
--  ^ beta-Reduktion
-- (Lz.z y)
--  ^ delta-Reduktion
-- y


-- (c)

-- Nur mit normal order reduction:
-- (Lx.y(Lz.(z z z)Lz.(z z z)))

-- Gar nicht:
-- (Lz.(z z)Lz.(z z))