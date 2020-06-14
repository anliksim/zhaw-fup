-- Aufgabe 4

-- (a)

-- (b)

-- Normal order reduction:
--
-- (Lx.(x y)Lz.z)
--  ^ beta-Reduktion
-- (Lz.z y)
--  ^ delta-Reduktion
-- y


-- (c)

-- Nur mit normal order reduction:
-- (Lx.y(Lz.(z z z)Lz.(z z z)))

-- Gar nicht:
-- (Lz.(z z)Lz.(z z))