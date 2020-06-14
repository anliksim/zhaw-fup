-- EDSL - embedded domain specific language
-- Terme sind echte Teilmende der host language

-- Shallow embedding: Grundform plus Funktionen

-- Deep embedding: Datentyp, Konstruktoren haben gleiche Signatur wie die Funktionen beim shallow embedding


-- newtype Shape = Shape {inside :: Point -> Bool}
--  ^
-- inside :: Point -> Shape -> Bool

-- Viel code für error handling -> Monads
