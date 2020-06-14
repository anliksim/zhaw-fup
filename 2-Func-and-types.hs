-- Variable ist zeitunabhängig (immutability)
-- Nebeneffekte vermeiden/isolieren

-- Referenzielle Transparenz
-- -> lazy evaluation
-- -> einfachere Programmverifikation
-- -> erleichtert die Beweisführung / Optimierung
-- -> equational reasoning


-- Records

data Customer = Customer
     { customerId :: Integer
     , name :: String
     }

-- hat automatisch getters
-- customerId :: Customer -> Integer
-- name :: Customer -> String

c1 = Customer { customerId = 1
    , name = "John"
    }

-- name c1
-- customerId c1

-- Eq typ, Customer eq based on id

instance Eq Customer where
    (==) (Customer id1 _) (Customer id2 _) = id1 == id2


-- Summentypen
data Shape
    = Rectangle Float Float
    | Square Float
    | Circle Float

-- lassen sich direkt mit Patterns dekonstruieren
area :: Shape -> Float
area (Rectangle a b) = a * b
area (Square a) = a * a
area (Circle r) = 3.14 * r * r

-- area $ Rectangle 1 2
-- area $ Circle 3


-- Rekursive Summentypen

data Tree a
    = Node (Tree a) a (Tree a)
    | Leaf a

depth :: Tree a -> Integer
depth (Node left _payload right) =
    1 + max (depth left) (depth right)
depth (Leaf _payload) = 1

-- depth $ Node (Leaf 1) 4 (Node (Leaf 4) 0 (Leaf 9))

-- Listen

-- [E1,E2]
-- 1:2:3:[] = [1,2,3]
-- [2..5] = [2,3,4,5]

-- List-Comprehension:
-- [ <Term in i> | i <- <Bereich,..,<Bedingung> ]

lc1 = [(i,j) | i <- [1..2], j <- ['a']]
lc2 = [i+2 | i <- [1..5], i `mod` 2 == 0]




