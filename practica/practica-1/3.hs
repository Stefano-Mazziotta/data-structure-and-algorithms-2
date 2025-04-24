import Data.Char (toUpper)
{--
3. Dar dos ejemplos de funciones que tengan los siguientes tipos:
--}

-- a) (Int →Int) →(Bool →Bool)
fnA1:: (Int -> Int) -> (Bool -> Bool)
fnA1 f = not

fnA2:: (Int -> Int) -> (Bool -> Bool)
fnA2 f = \b -> (f 1 > f 0) == b

-- b) Bool → (Int → Bool)
fnB1:: Bool -> (Int -> Bool)
fnB1 b = \x -> if b then x > 0 else x < 0

fnB2:: Bool -> (Int -> Bool)
fnB2 True = even
fnB2 False = odd

-- c) Char -> Char
fnC1::Char -> Char
fnC1 c = toUpper c

fnC2::Char -> Char
fnC2 c = if c `elem` "aeiouAEIOU" then '*' else c

-- d) Int-> (Int->Bool) -> [Int]
fnD1::Int -> (Int->Bool) -> [Int]
fnD1 n p = filter p [0..n]

fnD2::Int -> (Int->Bool) -> [Int]
fnD2 n p = foldr (\x acc -> if p x then x : acc else acc) [] [1..n]

{--
foldr recorre de derecha a izquierda, pero arma la lista en orden original si usás x : acc.
f 1 (f 2 (f 3 (f 4 (f 5 []))))

El predicado decide si el número entra o no.
El acumulador (acc) guarda los resultados que ya pasaron el filtro.
--}

-- e) [a] -> (a -> [b]) -> [b]
fnE1:: [a] -> (a -> [b]) -> [b]
fnE1 xs f = foldr (\x acc -> f x ++ acc) [] xs

-- f) [[a]] -> (a -> Bool) -> [a]
fnF1::[[a]] -> (a -> Bool) -> [a]
fnF1 xs p = foldr (\sublist acc -> filter p sublist ++ acc) [] xs

-- fnF1 [[1,2],[3,4,5],[6]] even

-- g) (a, b, c) -> Bool
fnG1:: (a,b,c) -> Bool
fnG1 (a,b,c) = False

-- h) (a, b, c) -> Int -> c
fnH1:: (a,b,c) -> Int -> c
fnH1 (_, _, z) _ = z

-- i) (a, a, a) -> Int -> a
fnI1::(a, a, a) -> Int -> a
fnI1 (x,y,z) 0 = x
fnI1 (x,y,z) 1 = y
fnI1 (x,y,z) 2 = z
fnI1 (x,y,z) _ = error "Fuera de rango"