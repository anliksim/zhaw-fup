-- Rekurson = Selbstbezug

-- Auswertung erfolgt rückwärts

-- Gleihungssysteme
--
-- f(0) = c
-- f(n+1) = G(f(n))
--
-- haben immer eindeutige Lösung (Induktionsbeweis)

-- Primitives Rekurisonschema
--
-- f(0, x) = c(x)
-- f(n+1, x) = G(f(n,x),n,x)

data Nat
  = Z
  | S Nat
  deriving (Show, Eq)

primRec
    :: (Nat -> Nat -> tuple -> Nat)
    -> (tuple -> Nat)
    -> Nat
    -> tuple
    -> Nat
primRec g c Z x = c x
primRec g c (S n) x = g (f n x) n x
    where
        f = primRec g c



-- Wertverlaufsrekursion

-- Bezug auf mehrere Vorgänger
-- z.B. fib

-- f(n) = G(f | n)
-- f(n, x) = G(f | n,n,x)

ord :: Nat -> [Nat]
ord = ord' []
    where
        ord' acc Z = acc
        ord' acc (S k) = ord' (k:acc) k

covRec :: ([Nat] -> Nat) -> Nat -> Nat
covRec g Z = g []
covRec g (S n) = g fs
    where
        f = covRec g
        fs = map f $ ord $ S n


-- Wohlfundiertheit:
-- Rekursion kann entlang jeder Relation angewendet werden, die keine unenlich absteigenden Ketten erlaubt


next :: Integer -> Integer
next n | n < 2 = 1
next n | n `mod` 2 == 0 = n `div` 2
next n = 3 * n + 1

colLength :: Integer -> Integer
colLength n | n < 2 = 1
colLength n = 1 + colLength (next n)

-- Strukturelle Rekursion - rekursive Typen dekonstruieren
length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

