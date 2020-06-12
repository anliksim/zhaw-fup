-- shallow embedding

newtype Polynomial = Polynomial { app :: Integer -> Integer }

addP :: Polynomial -> Polynomial -> Polynomial
addP (Polynomial p1) (Polynomial p2) = Polynomial $ \x -> p1 x + p2 x

monome :: Integer -> Integer -> Polynomial
monome coefficient exponent = Polynomial $ \x -> coefficient * x^exponent

sumNotation :: [Integer] -> Polynomial
sumNotation xs = Polynomial $
    \x -> sum (map (\(c,e) -> c*x^e) xsIndexed)
    where
        xsIndexed = zip xs [0..]

-- deep embedding

data DPolynomial
    = Fun (Integer -> Integer)
    | Monome Integer Integer
    | AddP DPolynomial DPolynomial
    | SumNotation [Integer]

eval :: DPolynomial -> Integer -> Integer
eval (Fun x) = x
eval (Monome c e) = \x -> c * x^e
eval (AddP p1 p2) = \x -> (eval p1 x) + (eval p2 x)
eval (SumNotation xs) = \x -> sum (map (\(c,e) -> c*x^e) xsIndexed)
      where
          xsIndexed = zip xs [0..]


-- function
pfun = (\x -> x * x + x)

-- shallow polynomial
p = (Polynomial pfun)
-- deep polyomial
dp = (Fun pfun)

-- shallow monome
m = monome 2 2
-- deep monome
dm = (Monome 2 2)

-- shallow addP
r1 = addP p p
r2 = addP m m
-- deep addP
dr1 = (AddP dp dp)
dr2 = (AddP dm dm)

-- shallow sumNotation
s = sumNotation [1,2,3,4]
-- deep sumNotation
ds = (SumNotation [1,2,3,4])

-- usage e.g.:
-- shallow: app s 3
-- deep: eval s 3


same x y = x == y

test = same (eval ds 2) (app s 2)
    && same (eval dr1 5) (app r1 5)
    && same (eval dr2 5) (app r2 5)
    && same (eval dm 3) (app m 3)
    && same (eval dp 1) (app p 1)

