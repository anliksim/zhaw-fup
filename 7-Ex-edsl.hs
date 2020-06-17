-- Aufgabe 2
import Data.List

type Point = (Float, Float)
type Vector = Point

data Matrix = Matrix (Float, Float) (Float, Float)
    deriving Show

scale :: Float -> Matrix -> Matrix
scale r (Matrix (a, b) (c, d)) = Matrix (r*a, r*b) (r*c, r*d)

invert :: Matrix -> Matrix
invert (Matrix (a, b) (c, d)) = scale
    (1/(a*d - b*c))
    (Matrix (d, -b) (-c, a))

apply :: Matrix -> Vector -> Vector
apply (Matrix (a, b) (c, d)) (x, y) =
    ( a*x + b*y
    , c*x + d*y
    )


data DShape
  = Empty
  | UnitDisc
  | UnitSq
  | Translate Point DShape
  | Negate DShape
  | Intersect DShape DShape
  | Merge DShape DShape
  | Minus DShape DShape
  | StretchX Float DShape
  | StretchY Float DShape
  | Stretch Float DShape
  | FlipX DShape
  | FlipY DShape
  | Flip45 DShape
  | Flip0 DShape
  | Rotate Float DShape


transformM :: Matrix -> DShape -> Point -> Bool
transformM m s = \(x, y) -> eval s $ apply (invert m) (x,y)

eval :: DShape -> Point -> Bool
eval (Empty) = \(_,_) -> False
eval (UnitDisc) = \(x, y) -> x^2 + y^2 <= 1
eval (UnitSq) = \(x, y) -> abs x <= 1 && abs y <= 1
eval (Translate (dx,dy) s) =  \(x, y) -> eval s (x - dx, y - dy)
eval (Negate s) = \(x, y) -> not $ eval s (x, y)
eval (Intersect s1 s2) = \(x, y) -> eval s1 (x, y) && eval s2 (x, y)
eval (Merge s1 s2) = \(x, y) -> eval s1 (x, y) || eval s2 (x, y)
eval (Minus s1 s2) = \(x, y) -> eval (Intersect s1 (Negate s2)) (x, y)
eval (StretchX r s) = \(x,y) -> transformM (Matrix (r, 0) (0, 1)) s (x,y)
eval (StretchY r s) = \(x,y) -> transformM (Matrix (1, 0) (0, r)) s (x,y)
eval (Stretch r s) = \(x,y) -> transformM (Matrix (r, 0) (0, r)) s (x,y)
eval (FlipX s) = \(x,y) -> transformM (Matrix (1, 0) (0, -1)) s (x,y)
eval (FlipY s) = \(x,y) -> transformM (Matrix (-1, 0) (0, 1)) s (x,y)
eval (Flip45 s) = \(x,y) -> transformM (Matrix (0, 1) (1, 0)) s (x,y)
eval (Flip0 s) = \(x,y) -> transformM (Matrix (-1, 0) (0, -1)) s (x,y)
eval (Rotate a s) = \(x,y) -> transformM (Matrix (cos a, -(sin a)) (sin a, cos a)) s (x,y)

em = (Empty)
-- eval em (1,2)
-- eval (FlipX em) (1,2)
-- eval (Stretch 1 em) (1,2)