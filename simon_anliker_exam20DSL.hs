-------------------------------------------------------------------------------
-- Gegeben ist folgende EDSL (shallow embedding) zum Spezifizieren von 
-- einfachen Funktionen
-------------------------------------------------------------------------------

-- Ziel der EDSL ist es, einfache Funktionen auszudrücken
type Func = Integer -> Integer

-- 'constF x' ist die Konstante funktion \_ -> x
constF :: Integer -> Func
constF x = const x

-- 'addF' ist ein Kombinator zum 'addieren' von Funktionen  
addF :: Func -> Func -> Func
addF f g x = f x + g x 

-- 'composeF' bezeichnet die klassische Funktionskomposition
composeF :: Func -> Func -> Func
composeF f g = f . g

-- 'casesF' erlaubt es eine Funktion durch Fallunterscheidung zu
-- definieren.
casesF :: Func -> Func -> Func -> Func
casesF f g h x 
    | f x == 0 = g x
    | otherwise = h x

-- 'repeatF' erlaubt es eine Funktion durch 'n' fache Anwendung zu
-- definieren
repeatF :: Integer -> Func -> Func
repeatF n f x 
    | n < 1 = x
    | otherwise = repeatF (n-1) f (f x)

-- Beispiel: f x = 2x
f :: Func
f = addF 
    (repeatF 0 (constF 0))
    (repeatF 0 (constF 0))


-------------------------------------------------------------------------------
-- Teil (a): Implementieren Sie die gegebene EDSL als 'Deep Embedding'
-- Beachten Sie die Komentare!
-------------------------------------------------------------------------------

-- AST Datentyp der EDSL
data FuncTerm
    = ConstF Integer
    | AddF FuncTerm FuncTerm
    | ComposeF FuncTerm FuncTerm
    | CasesF FuncTerm FuncTerm FuncTerm
    | RepeatF Integer FuncTerm

-- Semantik
apply :: FuncTerm -> (Integer -> Integer)
apply (ConstF x) = const x
apply (AddF f g) = \x -> f x + g x
apply (ComposeF f g) = f . g
apply (CasesF f g h) = \x -> cases_ f g h x
    where
      cases_ f g h x | f x == 0 = apply g x
      cases_ f g h x = apply h x
apply (RepeatF n f) = \x -> repeat_ n f x
    where
      repeat_ n f x | n < 1 = x
      repeat_ n f x = repeat_ (n-1) apply f (apply f x)


-- apply (ConstF 1) 2

-------------------------------------------------------------------------------
-- Teil (b): Implementieren Sie die in den Kommentaren gegebenen Funktionen
-- in ihrem Deep Embedding
-------------------------------------------------------------------------------

-- g x = x
g :: FuncTerm
g = (ConstF 0)


-- h x = 3x + 1
h :: FuncTerm
h = (AddF
      (RepeatF 3 (ConstF 0))
      (ConstF 1))


