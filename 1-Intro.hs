-- f x als f(x)
-- linksassoziativ -> f x y z ist (((f x) y) z)

-- Haskell:
-- statisch, stark typisiert
-- Typen als Signatur (fakultativ -> type inference)


-- File laden:
-- :l file.hs

-- Typ anzeigen:
-- :t sumF
-- sumF :: Num p => [p] -> p
sumF :: [Integer] -> Integer
sumF [] = 0
sumF (x:rest) = x + sumF rest


-- let oder where

g :: Integer -> Integer
g x =
  let
    sign
      | x < 0 = -1
      | x > 0 = 1
      | otherwise = 0
    in
     (x * x) * sign

-- ist gleich wie

g' :: Integer -> Integer
g' x = (x * x) * sign
  where
    sign
      | x < 0 = -1
      | x > 0 = 1
      | otherwise = 0


-- Funktion in Teildeklarationen

fibo :: Integer -> Integer
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- entspricht pattern matching

fibo' :: Integer -> Integer
fibo' n = case n of
    0 -> 1
    1 -> 1
    _ -> fibo (n-1) + fibo (n-2)


-- Funktion als Rückgabewert

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = f (g x) -- oder f $ g x

-- Lambdas

-- calcL x = 5 * (x + 10)
calcL = compose (\x -> 5 * x) (\x -> x + 10)


-- Infix-Notation

(-.-) :: Integer -> Integer -> Integer
(-.-) x y = (x * y) + 5
-- 2 -.- 3 -> 11

-- Same mit ticks für zweistellige Funktionen

o :: Integer -> Integer -> Integer
o x y = (x * y) + 5
-- 2 `o` 3 -> 11

