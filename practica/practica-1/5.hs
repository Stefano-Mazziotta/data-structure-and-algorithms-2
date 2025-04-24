-- 5. definir las siguientes funciones usando foldr:
-- a) map::(a → b) → [ a ] → [b ] que dada una funci ́on y una lista, aplica la función a cada elemento de la lista.
mymap::(a -> b) -> [a] -> [b]
mymap g xs = foldr (\y ys -> g y:ys) [] xs

-- b) filter :: (a → Bool) → [ a ] → [ a ]
-- dado un predicado y una lista xs, devuelve una lista con los elementos de xs que satisfacen el predicado.
myfilter::(a -> Bool) -> [a] -> [a]
myfilter p xs = foldr (\y ys -> if p y then y:ys else ys) [] xs

-- c) unzip :: [(a , b)] →([a ], [ b ]), 
-- Dada una lista de tuplas xs retorna una tupla de listas 
-- donde cada una corresponde a los primeros y segundos elementos de los pares respectivamente.
-- Ej. unzip [(’a’, 1), (’z’, 7), (’h’, 9)] = ("azh", [1, 7, 9])

myunzip::[(a,b)] -> ([a],[b])
myunzip xs = foldr (\(a, b) (as, bs) -> (a:as, b:bs)) ([], []) xs

-- d) pair2List::(a,[b]) -> [(a,b)] 
-- dado un par formado por un valor x y una lista xs convierta a la lista xs en una lista de pares,
-- formada con los elementos de xs y x.

-- Ej. pair2List (x , [ y1 , y2 , y3 ]) = [(x , y1 ), (x , y2 ), (x , y3 )]
pair2List::(a,[b]) -> [(a,b)]
pair2List (x, xs) = foldr (\y ys -> (x, y):ys) [] xs

{--
e) maxSec :: [ (Int, Int)] → (Int, Int)
Dada una lista de pares de naturales que represente a una lista de segmentos de la recta,
calcule el segmento más largo de la misma.

Ej.maxSec [(1, 2), (0, 7), (4, 6)] = (0, 7)

Puede definir una función auxiliar maxL::(Int, Int)-> (Int, Int) -> (Int, Int), 
Dados dos pares de naturales que representan a dos segmentos de la recta,
devuelva el segmento cuya longitud sea máxima.
Ej.maxL (1, 2) (0, 7) = (0, 7).
--}

maxL::(Int, Int) -> (Int, Int) -> (Int, Int)
maxL (a, b) (x,y) = 
    let longitudA = b-a
        longitudB = y-x
    in if longitudA > longitudB then (a,b) else (x,y)

maxSec::[(Int, Int)] -> (Int, Int)
maxSec xs = foldr (\(x,y) (acc1,acc2) -> maxL (x,y) (acc1, acc2)) (0,0) xs