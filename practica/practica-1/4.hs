-- 4. Dar el tipo de la siguiente funciones o expresiones:
-- a)
(∧) :: Bool -> Bool -> Bool
a ∧ b = a && b

foo1:: Bool -> (Bool -> Bool)
foo1 p = if p then (p ∧) else (p ∧)

--b)
foo2::(b -> c) -> (a -> b) -> a -> c
foo2 x y z = x (y z)

-- c) 
foo3::(a -> c -> d) -> a -> c -> d
foo3 x y z = x y z

-- d) 
foo4::(b -> a) -> b -> [a] -> [a]
foo4 x y z = x y : z


-- e)
foo5::b -> (a -> [b]) -> a -> [b]
foo5 x y z = x : y z

-- f)
foo6:: [a] -> (b -> [a]) -> b -> [a]
foo6 x y z = x ++ y z

-- g)
foo7:: [[a]] -> ([[a]] -> Bool) -> [a]
foo7 a b = if b a then head a else [ ]

-- h)
foo8::[a] -> ([a] -> Bool) -> [a]
foo8 a b = if b a then a else [ ]

-- i) muuy rara esta funcion
-- foo9 :: a -> (a -> Bool) -> [a]
-- foo9 a b = if b a then head (:a) else (:[ ])
