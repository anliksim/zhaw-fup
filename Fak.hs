

fak :: Integer -> Integer
fak n | n <= 0 = 1
fak n = n * fak (n-1)


-- Fakultät mit Akkumulator
-- Akkumulator Pattern
fakTR :: Integer -> Integer
fakTR = fakTR_ 1
    where
        fakTR_ :: Integer -> Integer -> Integer
        fakTR_ acc 0 = acc
        fakTR_ acc n = fakTR_ (n * acc) (n-1)


-- Continuation Pattern
-- Funktionsparameter mit noch zu leistende Arbeit

fakC :: Integer -> Integer
fakC = fakC_ (const 1)
  where
    -- fakTR_ acc 0 = acc
    -- fakTR_ acc n = fakTR_ (n * acc) (n-1)
    fakC_ :: (Integer -> Integer) -> Integer -> Integer
    fakC_ f n | n < 1 = f n
    fakC_ f n = fakC_ (\x -> n * (f x)) (n-1)
