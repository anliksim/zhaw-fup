-- Aufgabe 2
import Data.List

type Point = (Float, Float)
type Vector = Point

data Matrix = Matrix (Float, Float) (Float, Float)
    deriving Show

data DShape
  = Empty
  | UnitDisc
  | UnitSq
  | Translate Point DShape
  | Negate DShape
  | Intersect DShape DShape
  | Merge DShape DShape
  | Minus DShape DShape

eval :: DShape -> Point -> Bool
eval (Empty) = \(_,_) -> False
eval (UnitDisc) = \(x, y) -> x^2 + y^2 <= 1
eval (UnitSq) = \(x, y) -> abs x <= 1 && abs y <= 1
eval (Translate (dx,dy) s) =  \(x, y) -> eval s (x - dx, y - dy)
eval (Negate s) = \(x, y) -> not $ eval s (x, y)
eval (Intersect s1 s2) = \(x, y) -> eval s1 (x, y) && eval s2 (x, y)
eval (Merge s1 s2) = \(x, y) -> eval s1 (x, y) || eval s2 (x, y)
eval (Minus s1 s2) = \(x, y) -> eval (Intersect s1 (Negate s2)) (x, y)
