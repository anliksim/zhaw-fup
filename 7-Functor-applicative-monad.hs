-- Functor

-- class Functor f where
--    fmap :: (a -> b) -> f a -> f b

-- Infix
-- (<$>) = fmap

r1 = (+1) <$> [1,2,3]
-- [2,3,4]

-- Regeln Functor

-- Identität:
-- fmap id = id
-- id <$> x = x

-- Komposition:
-- fmap (f . g) = (fmap f) . (fmap g)
-- (f . g) <$> x = f <$> (g <$> x)

predec :: Int -> Maybe Int
predec x | x <=1 = Nothing
predec x = Just $ x - 1

-- Maybe ist ein Functor -> kombination mit Funktion welche Int anstatt Maybe Int erwartet
doublePredec :: Int -> Maybe Int
doublePredec x = (*2) <$> predec x


-- geht nicht mit zweistelligen Funktionen
-- h x y = (*) <$> (predec x) (predec y)


-- Applicative Functor

-- wir brauchen eine Fuktion vom Typ
-- (<*>) :: Maybe (Int -> Int) -> Maybe Int -> Maybe Int

prodPredec :: Int -> Int -> Maybe Int
prodPredec x y = (*) <$> predec x <*> predec y

-- class (Functor f) => Applicative f where
--    pure  :: a -> f a
--    (<*>) :: f (a -> b) -> f a -> f b

-- pure = Just
-- Just f <*> x = f <$> x
-- Nothing <*> _ = Nothing

-- geht für n-Stellige Funktionen

mul3 x y z = x * y * z

d x y z = mul3 <$> predec x <*> predec y <*> predec z

-- Regeln App. Functor

-- Identität:
-- pure id <*> vv = vv

-- Komposition:
-- pure (.) <*> f <*> g <*> x = f <*> (g <*> x)

-- Homomorphismus:
-- pure f <*> pure v = pure (f v)

-- Interchange:
-- f <*> pure x = pure ($ x) <*> f


-- Monad

-- Profile + CityBase für das bauen von User
-- wir brauchen die Email von Profile für den CityBase lookup, somit eine Funktion vom Typ

-- bind :: Maybe String -> (String -> Maybe String) -> Maybe String

-- für chaining von Funtkionen mit Typ
-- String -> Maybe String

-- class (Applicative m) => Monad m where
--     (>>=) :: m a -> (a -> m b) -> m b


-- Regeln Monad

-- left identity:
-- pure a >>= f = f a

-- right identity:
-- m >>= pure = m

-- Assoziativität:
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)

-- pure im Zusammenhang mit Monad auch: return