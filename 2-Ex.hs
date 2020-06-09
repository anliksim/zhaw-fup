-- Praktikum 2

-- Aufgabe 1

-- (a)

data BTree a
    = BNode (BTree a) a (BTree a)
    | Leaf a

collect :: BTree a -> [a]
collect (Leaf a) = [a]
collect (BNode left a right) =
    (collect left) ++ [a] ++ (collect right)

bTree :: BTree Integer
bTree = BNode (Leaf 2) 1 (Leaf 3)

bTreeList = collect bTree

-- (b)


-- Aufgabe 2

-- Aufgabe 3

