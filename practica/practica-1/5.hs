-- 5. definir las siguientes funciones usando foldr:
-- a) map::(a → b) → [ a ] → [b ] que dada una funci ́on y una lista, aplica la función a cada elemento de la lista.
map'

{--
b) filter :: (a → Bool) → [ a ] → [ a ] , que dado un predicado y una lista xs, devuelve una lista
con los elementos de xs que satisfacen el predicado.

c) unzip :: [(a , b)] →([a ], [ b ]), que dada una lista de tuplas xs retorna una tupla de listas donde
cada una corresponde a los primeros y secundos elementos de los pares respectivamente.
Ej. unzip [(’a’, 1), (’z’, 7), (’h’, 9)] = ("azh", [1, 7, 9])

d) pair2List ::(a , [ b ]) →[(a , b)] que dado un par formado por un valor x y una lista xs convierta
a la lista xs en una lista de pares, formada con los elementos de xs y x .
Ej. pair2List (x , [ y1 , y2 , y3 ]) = [(x , y1 ), (x , y2 ), (x , y3 )]

e) maxSec :: [ (Int, Int)] → (Int, Int), que dada una lista de pares de naturales que represente a
una lista de segmentos de la recta, calcule el segmento m ́as largo de la misma.
Ej.maxSec [(1, 2), (0, 7), (4, 6)] = (0, 7)
Puede definir una funci ́on auxiliar maxL :: (Int, Int) → (Int, Int) → (Int, Int), que dados dos
pares de naturales que representan a dos segmentos de la recta, devuelva el segmento cuya
longitud sea m ́axima.
Ej.maxL (1, 2) (0, 7) = (0, 7).
Pr ́actica Haskell P ́agina 3
--}