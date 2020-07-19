-- Aufgabe 1

-- (a)

-- List muss durchlaufen werden - n Schritte anstatt 1


-- (b)

mkLists :: Integer -> [[Integer]]
mkLists 0 = []
mkLists n = mkLists(n-1) ++ [[1..n]]

mkl1 = mkLists 1
mkl2 = mkLists 2
mkl3 = mkLists 3

mkLists' n = [[1..k] | k <- [1..n]]

-- (c)

t = \_ x -> x
y = undefined
--foldl t y xs == last xs
-- > True

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

-- Beweis D.
--
-- Fallunterscheidung:
--
-- kein Element
-- last1 [] = undefined
-- last2 [] = head (reverse []) = head [] = undefined
--
-- ein Element
-- last1 [x] = x
-- last2 [x] = head (reverse [x]) = head [x] = x
--
-- generell
-- last1 (x:y:xs) = last1 (y:xs)
--                = last2 (y:xs) // Induction hypothesis
--
-- last2 (x:y:xs) = head (reverse (x:y:xs))
--                = head (reverse xs ++ [y,x])
--                = head ((reverse xs ++ [y]) ++ [x])
--                = head (reverse xs ++ [y])
--                = head (reverse (y:xs))
--                = last2 (y:xs)


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

data DTerm
    = Constant Integer
    | Sum DTerm DTerm
    | Product DTerm DTerm

eval :: DTerm -> Integer
eval (Constant x) = x
eval (Sum t1 t2) = (eval t1) + (eval t2)
eval (Product t1 t2) = (eval t1) * (eval t2)

-- (iii)

aTerm :: DTerm
-- (2 + (3 + 4)) * 7
aTerm = Product (Sum (Constant 2) (Sum (Constant 3) (Constant 4))) (Constant 7)


-- Aufgabe 3

-- (a)

f :: Integer -> Integer
f 0 = 0
f n = n + f (n-1)

fTR :: Integer -> Integer
fTR = fTR_ 0
    where
      fTR_ acc 0 = acc
      fTR_ acc n = fTR_ (n+acc) (n-1)


-- (b)

-- avg von Listen

avgTR :: [Float] -> Float
avgTR = avgTR_ (0,0)
    where
--      avgTR_ (0,sum) [] = 0 -- definiert bei leere Liste?
      avgTR_ (n,sum) [] = sum / n
      avgTR_ (n,sum) (x:xs) = avgTR_ (n+1,sum+x) xs

-- (c)

colTR :: Integer -> [Integer]
colTR x = reverse $ colTR_ [] x
    where
      next n | n `mod` 2 == 0 = n `div` 2
      next n = 3 * n + 1
      colTR_ xs n | n <= 1 = n:xs
      colTR_ xs n = colTR_ (n:xs) (next n)

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