-- Lambda Kalkül

-- Evaluation of expressions
-- Turing vollständig
-- höhere Funktionen - programs as data duality

-- Zutaten:
-- Terme (Programme)
-- Regeln zur Manipulation von Termen,
-- Typensystem (eventuell)

-- Lambda-Terme (L als Lambda-Symbol):
-- Jede Variable und Konstante ist ein Term
-- Applikation: Sind A und B Terme, dann auch (A B)
-- Abstraktion: Ist x eine Variable und A ein Term, dann ist auch Lx.A ein Term

-- (A B) -> Anwendung von A auf B wie z.B (f x)
-- Lx.A -> anonyme Funktion (\x -> A)
-- Lx.(x x) lässt sich nicht in Haskell realisieren wegen Typisierung


data ETerm
    = Add
    | N Integer
    | EVar String
    | EApp ETerm ETerm
    | EAbs String ETerm

pretty :: ETerm -> String
pretty (Add) = "Add"
pretty (N x) = show x
pretty (EVar s) = s
pretty (EApp t1 t2) = "(" ++ (pretty t1) ++ " " ++ (pretty t2) ++ ")"
pretty (EAbs s t) = "L" ++ s ++ "." ++ (pretty t)

yTerm =
    EAbs "x" $ EAbs "y" $
       EApp
        (EApp Add (EVar "x"))
        (EVar "y")


-- Konventionen

-- Äussere Klammern weglassen
-- A ist (A)

-- Applikation is linksassoziativ
-- A B C ist ((A B) C)

-- Bereich einer quantifizierten Variable ist grösstmöglich
-- Lx.A B C ist Lx.((A B) C)

-- Lxyz.A ist Lx.Ly.Lz.A


-- Lxy.ABLu.A
-- Lx.(Ly.(AB))Lu.(A)

-- Substitution
-- zulässig wenn keine freie Varible gebunden
-- unzulässig: Lxy.(x y z)[z:=x]
-- zulässig:   Lxy.(x y z)[z:=u]


-- Konversion -> vereinfachen eines Terms
-- Normalform -> nicht mehr zu vereinfachenden Term


-- alpha-Konversion
-- äquvalente Terme ineinander überführen
-- Bedeutung von Term hängt von Struktur und nicht Variablen ab

-- Lx.A => Ly.A[x:=y] (wenn y nicht in A vorkommt)
-- z.B. Lfgx.(g f x) => Lhgx.(g h x)


-- beta-Konversion
-- Funktionsanwendung durch ersetzen von Werten durch Funktionswerte

-- (Lx.A B) => A[x:=B] (Substitution muss zulässig sein)
-- z.B. (L.x.(Add x x) z) => (Add z z)


-- eta-Konversion
-- Funktionen die gleichen Rückgabewert produzieren sind gleich

-- (Lx.A x) => A (x nicht element von FV(A), A keine Konstante)
-- z.B. Lx.(Add y x) => (Add y)


-- delta-Konversion
-- bestimmt Bedeutung der Konstanten

-- (Add 14) 3 => 17


-- Beispiel
-- Lf.Lx.(f (f x)) (Add 3) 2
--  ^ beta-Reduktion
-- Lx.((Add 3) ((Add 3) x)) 2
--  ^ beta-Reduktion
-- ((Add 3) ((Add 3) 2))
--  ^ delta-Reduktion
-- ((Add 3) 5)
--  ^ delta-Reduktion
-- 8


-- delta-Reduktion: Übergang Term A zu B durch delta-Konversion, in B Wert der Konstante eingesetzt

-- beta-Reduktion: Übergang von (Lx.A B) zu A[x:=B] mit beta-Konversion

-- beta-Redex: Term auf den wir beta-Reduktion anwenden können

-- beta-Normalform: Wenn kein beta-Redex als Teilterm und keine delta-Reduktion mehr möglich

-- Ein Term A evaluiert zu Term B, wenn B in beta-Normalform ist und eine endliche Sequenz von Reduktionen von A nach B existiert

-- Anzahl Redexe verringern sich mit Reduktion nicht notwendigerweise:
-- Lf.(f f f)(Lx.A)
--  ^ beta-Reduktion
-- (Lx.A)(Lx.A)(Lx.A)

-- nicht jeder Term kann zu beta-Normalform reduziert werden
-- Lx.(x x)Lx.(x x)


-- Reduktionsreihenfolge nicht eindeutig

-- Lx.y(Lw.(w w)Lx.x)

-- Normal order reduction (von links nach rechts)
-- (z.B. lazy evaluation)

-- Lx.y(Lw.(w w)Lx.x)
--  ^beta-Reduktion
-- Lx.y(Lx.xLx.x)
--  ^beta-Reduktion
-- Lx.yLx.x
--  ^beta-Reduktion
-- y

-- Applicative order reduction (von innen nach aussen)
-- (Argumente werden zuerst evaluiert, z.B. strict evaluation)

-- Lx.y(Lw.(w w)Lx.x)
--  ^beta-Reduktion
-- y


-- Wenn ein Term eine beta-Normalform besitzt:
-- > wird immer durch normal order reduction gefunden
-- > nicht immer durch applicative order reduction
-- > ist sie immer eindeutig (bis auf alpha-Konversion)

-- Rekursion mit Lambda-Kalkülen
-- Fixpunktkombinator Y
-- Y := Lf.(Lx.(f (x x))Lx.(f (x x)))

-- Fixpunkteigenschaft (Y g) = (g (Y g))

-- Lambda-Kalkül ist Turing-vollständig
-- > While Schleifen via Rekurison
-- > Natürliche Zahlen via Church Numeral
-- > Paare und Projektionen
-- > Listen via Paare





