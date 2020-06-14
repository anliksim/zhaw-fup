-- Fixpunkte

-- konstruktiver Zugang zu rekursiv definierten Funktionen
-- Aufbauen einer rekursiven Struktur von Unten

-- Fixpunkte einer Funktion -> Elemente x - F(x) = x

expF :: (Integer -> Integer) -> Integer -> Integer
expF f x | x == 0 = 1
expF f x = 2 * f (x - 1)

e :: Integer -> Integer
e x = undefined

-- e 0
-- > error undefined
-- expF e 0
-- > 1
-- expF e 1
-- > error undefined
-- expF (expF e) 1
-- > 2

fix :: ((Integer -> Integer) -> Integer -> Integer) -> Integer -> Integer
fix fn = fn $ fix fn

expFix :: Integer -> Integer
expFix = fix expF


-- Endrekursion / tail recursion

-- wenn Resultate von rekursiven Aufrungen direkt zurückgegeben werden

-- tail-call Optimierung -> konstanter Stack Speicher

sum_ :: [Integer] -> Integer
sum_ [] = 0
sum_ (x:xs) = x + (sum_ xs)
-- nicht Endrekursiv da Summenbildung
-- kann nur "von hinten" ausgewertet werden und muss daher den ganzen Stack aufbauen

-- sum mit Akkumulator Pattern
-- das Resultat durch den Akkumulator mitgegeben
sumTR_ :: Integer -> [Integer] -> Integer
sumTR_ acc [] = acc
sumTR_ acc (x:xs) = sumTR_ (x + acc) xs

-- Akkumulator startet bei 0
sumTR = sumTR_ 0


-- Fakultät mit Akkumulator
-- Akkumulator Pattern
fakTR :: Integer -> Integer
fakTR = fakTR_ 1
    where
        fakTR_ :: Integer -> Integer -> Integer
        fakTR_ acc 0 = acc
        fakTR_ acc n = fakTR_ (n * acc) (n-1)


-- nicht tr
pow :: Integer -> Integer -> Integer
pow x y | y < 1 = 1
pow x y = x * pow x (y -1)

-- tr
powTR :: Integer -> Integer -> Integer
powTR = powTR_ 1
    where
        powTR_ :: Integer -> Integer -> Integer -> Integer
        powTR_ acc x y | y < 1 = acc
        powTR_ acc x y = powTR_ (x * acc) x (y-1)


-- nicht tr
isPalindrome :: String -> Bool
isPalindrome w | length w < 2 = True
isPalindrome w = w0 == wE && isPalindrome w'
    where
        w0 = head w
        wE = last w
        w' = tail $ init w

-- tr
isPalindromeTR :: String -> Bool
isPalindromeTR = pTR_ True
    where
        pTR_ :: Bool -> String -> Bool
        pTR_ acc w | length w < 2 = acc
        pTR_ acc w = pTR_ (acc && w0 == wE) w'
            where
              w0 = head w
              wE = last w
              w' = tail $ init w


-- Akkumulator repräsentiert bereits geleistete Arbeit

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


-- nicht tr
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = (f x):(myMap f xs)

-- tr
myMapTR :: (a -> b) -> [a] -> [b]
myMapTR f x = myMapTR_ id f (reverse x)
    where
      myMapTR_ cont f [] = cont []
      myMapTR_ cont f (x:xs) = myMapTR_
        (\y -> (f x):(cont y)) f xs

myMapTR' :: (a -> b) -> [a] -> [b]
myMapTR' = myMapTR_ id
    where
      myMapTR_ cont f [] = cont []
      myMapTR_ cont f (x:xs) = myMapTR_ cont' f xs
          where
              cont' y = cont ((f x):y)
