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

