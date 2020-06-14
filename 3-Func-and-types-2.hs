-- Reine Funktion
-- -> für jede Eingabe x genau eine Ausgabe f(x)
f :: Integer -> Integer
f x = 3 * x

-- Funktionsdeklaration ist rechtsassoziativ
-- a -> b -> c -> d
-- a -> (b -> (c -> d)))


-- Mehrstellige Funktionen

-- Variante 1: n-Stellige Tupel als Input

max1 :: (Int, Int) -> Int
max1 (x, y) = if x > y then x else y

-- Variante 2: n 1-Stellige Funktionen als Output

-- Int -> (Int -> Int)
max2 :: Int -> Int -> Int
max2 x y = if x > y then x else y


-- Currying/Uncurrying ist die Übersetzung dieser Typen
-- curry f a1 .. an = f (a1,..,an)
-- uncurry f (a1,..,an) = f a1 .. an

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a,b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a,b) = f a b

-- Partielle Anwendung

plus4 :: Num a => a -> a
plus4 = (+) 4

-- curried Funktion nicht erschöpfend mit Argumenten versehen
-- normale Anwendung von mehrstelligen Funktionen


-- Funktion höherer Ordnung

-- Funktion, die Funktionen als Argumente erhält oder zurückgibt


-- höhere Funkiton zum doppelten Anwenden einer Funktion
twice :: (a -> a) -> a -> a
twice f x = f $ f x

-- Komposition
--(.) :: (b -> c) -> (a -> b) -> a -> c
--(.) f g x = f $ g x

twice' :: (a -> a) -> a -> a
twice' f = f . f

-- twice' (\x -> x + x) 2

-- n-times
many :: Int -> (a -> a) -> a -> a
many 1 f = f
many n f = f . many (n-1) f

-- many 5 (\x -> x + x) 2


-- Partielle Funktoin

-- gibt eventuell für gewisse Eingabe keinen Funktionswert zurück
-- z.B. f(x,y) = x/y

-- Summentyp Maybe für partielle/optionale Rückgabewerte
-- data Maybe a = Just a | Nothing

divM :: (Eq a, Fractional a) => a -> a -> Maybe a
divM x 0 = Nothing
divM x y = Just (x / y)

-- Either a b -> für mehrere Rückgabewerte z.B error msg
-- a error
-- b result

