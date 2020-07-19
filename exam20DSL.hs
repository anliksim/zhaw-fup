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
    = Todo1     -- Anpassen
    | Todo2     -- Anpassen
    | TodoETC   -- Anpassen.. Falls nötig auch um weitere Fälle erweitern.

-- Semantik
apply :: FuncTerm -> (Integer -> Integer)
apply t = undefined


-------------------------------------------------------------------------------
-- Teil (b): Implementieren Sie die in den Kommentaren gegebenen Funktionen
-- in ihrem Deep Embedding
-------------------------------------------------------------------------------

-- g x = x
g :: FuncTerm
g = undefined


-- h x = 3x + 1
h :: FuncTerm
h = undefined


