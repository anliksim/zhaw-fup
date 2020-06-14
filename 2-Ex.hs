-- Praktikum 2

-- Aufgabe 1

-- (a)

data BTree a
    = BNode (BTree a) a (BTree a)
    | BLeaf a

collectB :: BTree a -> [a]
collectB (BLeaf a) = [a]
collectB (BNode left a right) =
    (collectB left) ++ [a] ++ (collectB right)

bTree :: BTree Integer
bTree = BNode (BLeaf 2) 1 (BLeaf 3)

bTreeList = collectB bTree

-- (b)

data Tree a
  = Node a [Tree a]

collect :: Tree a -> [a]
collect (Node a []) = [a]
collect (Node a t) = collect (head t)
  ++ collect (Node a (tail t))

tree :: Tree Integer
tree =
    Node 1
      [ Node 2 []
      , Node 3 []
      , Node 4
        [ Node 5 []
        , Node 6 []
        ]
      ]

treeListSingle = collect (Node 2 [])
treeList = collect tree

-- Aufgabe 2


data NatNumber
  = Zero
  | Succ NatNumber
  deriving (Show, Eq)

eval :: NatNumber -> Integer
eval (Zero) = 0
eval (Succ x) = 1 + (eval x)

interpret :: Integer -> NatNumber
interpret 0 = Zero
interpret n = Succ (interpret (n-1))

--(.+) :: NatNumber -> NatNumber -> NatNumber
--(.+) x Zero = x
--(.+) Zero y = y
--(.+) x y = interpret $ (eval x) + (eval y)
--
--(.*) :: NatNumber -> NatNumber -> NatNumber
--(.*) x Zero = Zero
--(.*) Zero y = Zero
--(.*) x y = interpret $ (eval x) * (eval y)

(.+) :: NatNumber -> NatNumber -> NatNumber
(.+) Zero m = m
(.+) (Succ n) m = n .+ (Succ m)

(.*) :: NatNumber -> NatNumber -> NatNumber
(.*) Zero n = Zero
(.*) (Succ n) m = (n .* m) .+ m

fact :: NatNumber -> NatNumber
fact Zero = Succ Zero
fact (Succ n) = Succ n .* fact(n)

-- Aufgabe 3

-- fractran interpreter
--execute :: Program -> Input -> Output

