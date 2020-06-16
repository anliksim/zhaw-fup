-- Aufgabe 4

-- (a)

-- (b)

-- (i)
--
-- (Lx.(x x) y)
--  ^ beta-Reduktion (normal order)
-- (y y)
--
-- > beta-Normalform


-- (ii)
--
-- (y Lx.(x x))
--
-- > beta-Normalform


-- (iii)
--
-- (Lx.(x x x) Lx.(x x x))
--
-- > nicht beta-Normalform


-- (c)

-- t = (((Lxyz.((x y) (xz)) Lx.a) b) c)

-- (i)
-- a,b,c

-- (ii)
--
-- (((Lxyz.((x y) (x z)) Lx.a) b) c)
-- ((Lyz.((Lx.a y) (Lx.a z)) b) c)
-- ((Lx.a b) (Lx.a c))
-- (a (Lx.a c))
-- (a a)


