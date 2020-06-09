-- Praktikum 1

-- Aufagbe 1

-- square(n) = n^2
square :: Integer -> Integer
square n = n * n

-- exp(n) = 3^n
exp3 :: Integer -> Integer
exp3 0 = 1
exp3 n = 3 * exp3(n-1)

-- makePalindrome(w) = ww^-1
makePalindrome :: String -> String
makePalindrome w = w ++ reverse w

-- sum(f,m,n) = SUM i=m to n, f(i)
sumF :: (Integer -> Integer) -> Integer -> Integer -> Integer
sumF f m n = foldl (+) 0 [f i | i <- [m..n]]

-- Aufgabe 2

-- sumSq(m,n) = SUM i=m to n, i^2 mit sum und square
sumSq :: Integer -> Integer -> Integer
sumSq m n = sumF square m n

-- Aufgabe 3

-- Suchfunktion welche entscheidet ob das erste Argument ein Teilstring des zweiten Argumentes ist
isSubstring :: String -> String -> Bool
isSubstring substring string
  | (length string) < (length substring) = False
  | take (length substring) string == substring = True
  | otherwise = isSubstring substring (tail string)

-- short version
isSub :: String -> String -> Bool
isSub sub str =
  (sublen <= strlen &&
    (take sublen str == sub || isSub sub (tail str)))
  where
    sublen = length sub
    strlen = length str


-- Aufgabe 4

-- Replace pattern in a string
replace :: String -> String -> String -> String
replace pattern newPattern str
    | lp == 0 = str -- Nothing to replace
    | lp > ls = str -- Nothing to replace
    | take lp str == pattern = -- Here replacement happens
        newPattern ++ recurse (drop lp str)
    | otherwise = head str : recurse (tail str) -- Nothing to replace but maybe later on
    where
      lp = length pattern
      ls = length str
      recurse = replace pattern newPattern


-- Slightly more general and tail recursive version
replaceAcc :: (String -> String) -> String -> String -> String -> String
replaceAcc transform pattern acc "" = acc
replaceAcc transform pattern acc str
    | pattern == "" = str
    | lp > ls = acc ++ str
    | take lp str == pattern =
        replaceAcc transform pattern (acc ++ transformed) (drop lp str)
    | otherwise = replaceAcc transform pattern (acc ++ [head str]) (tail str)
    where
      lp = length pattern
      ls = length str
      transformed = transform pattern


wrapParens :: String -> String
wrapParens x = "<" ++ x ++ ">"

putInParens :: String -> String -> String
putInParens pattern s = replaceAcc wrapParens pattern "" s

-- (a)
-- ersetzt in alle "a" durch "<a>" in einem String
parens :: String -> String
parens s = replace "a" "<a>" s

parens' :: String -> String
parens' s = putInParens "a" s

-- (b)
-- ersetze alle "abc" durch "<abc>"
parens2 :: String -> String
parens2 s = replace "abc" "<abc>" s

parens2' :: String -> String
parens2' s = putInParens "abc" s



