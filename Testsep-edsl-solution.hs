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


-- Welcher Term entspricht dem Polynom 'x^2+2'

--    (+)  (1*x^2)     (2*x^1)
a1 = addP (monome 1 2) (monome 2 0)

-- Geben Sie zwei verschiedene Terme an, die das Polynom 'x - x^3' beschreiben

--    (+)  (1*x^1)      (-1*x^3)
a21 = addP (monome 1 1) (monome (-1) 3)

--    (+) (0*x^0) (1*x^1) (0*x^2) (-1*x^3)
a22 = sumNotation [0,1,0,(-1)]