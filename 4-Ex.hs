-- Aufgabe 1

-- hanoi:
--
--funktion bewege (Zahl i, Stab a, Stab b, Stab c) {
--    falls (i > 0) {
--       bewege(i-1, a, c, b);
--       verschiebe oberste Scheibe von a nach c;
--       bewege(i-1, b, a, c);
--    }
--}
hanoi :: Integer -> String -> String -> String -> [(String, String)]
hanoi 0 a b c = []
hanoi n a b c = h a c b ++ [(a, c)] ++ h b a c
  where
    h :: String -> String -> String -> [(String, String)]
    h = hanoi (n-1)

hanoi' :: Integer -> [(String, String)]
hanoi' n = hanoi n "a" "b" "c"


-- Aufgabe 2

-- exp(x,y) = x^y

primRec :: (Integer -> Integer -> Integer -> Integer)
     -> (Integer -> Integer) -> Integer -> Integer -> Integer
primRec g c 0 x = c x
primRec g c n x = g (f (n-1) x) n x
    where
        f = primRec g c

expPR :: Integer -> Integer -> Integer
expPR x y = primRec g c y x
      where
        c = (const 1) -- x^0 = 1
        g c _ x = c * x -- x * ... * x^0


-- Aufgabe 3

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


covRec g n = g [f i | i <- [0..(n-1)]]
    where
        f = covRec g

fibSC = covRec g
    where
        g []  = 0
        g [_] = 1
        g xs  = second_last + last
          where
            last = head (reverse (init xs))
            second_last = head (reverse xs)


-- Aufgabe 4

-- length'

-- Aufgabe 5

sieve :: (a -> a -> Bool) -> [a] -> [a]
sieve pred xs = case xs of
    [] -> []
    x:xs -> x:(sieve pred $ filter (pred x) xs)

sieveA :: (a -> a -> Bool) -> [a] -> [a]
sieveA = sieveA_ []
    where
      sieveA_ acc pred [] = reverse acc
      sieveA_ acc pred (x:xs) = sieveA_ (x:acc) pred xsn
        where
          xsn = filter (pred x) xs

sieveC :: (a -> a -> Bool) -> [a] -> [a]
sieveC = sieveC_ id
    where
      sieveC_ cont pred [] = cont []
      sieveC_ cont pred (x:xs) = sieveC_ cont' pred xsn
        where
          cont' = (\y -> cont (x:y))
          xsn = filter (pred x) xs


--sieve (<=) [1,2,3,4,1,5]
-- > [1,2,3,4,5]
--sieve (==) [1,2,3,4,1,5]
--sieve (\x -> \y -> x == y) [1,2,3,4,1,5]
-- > [1,1]

p x y = y `mod` x /= 0
primes n = sieve p [2..n]

primesF f n = primesF_ f n
    where
        primesF_ f n | n < 2 = []
        primesF_ f n | all check ps = n:ps
        primesF_ f n | otherwise = ps
        ps = f $ n-1
        check p = n `mod` p /= 0

fix f x = f (fix f) x
primes' = reverse . fix primesF