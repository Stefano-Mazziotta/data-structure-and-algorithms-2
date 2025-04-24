{--
2. Definir usando listas por comprensión las funciones:

a) cambios : [a] → [Int], que dada una lista, devuelve la lista de los ı́ndices en que la lista cambia.
Es decir, dada la lista S retorna la lista con los i tal que S[i] != S[i+1]
cambios [1, 1, 1, 3, 3, 1, 1] = [2, 4]
--}


-- cambios [1] = []
-- cambios [1,2] = [0]
-- cambios [1,1,3] = [1]
-- cambios [1,1,3,4] = [1,2]  


cambios :: Eq a => [a] -> [Int]
cambios xs = [i | i <- [0 .. length xs - 2], xs !! i /= xs !! (i + 1)]

{--
otra forma
cambios :: Eq a => [a] -> [Int]
cambios xs = [i | (i, (x, y)) <- zip [0..] (zip xs (tail xs)), x /= y]

cambios [1, 1, 1, 3, 3, 1, 1]
-- zip xs (tail xs)     => [(1,1), (1,1), (1,3), (3,3), (3,1), (1,1)]
-- zip [0..] ...        => [(0,(1,1)), (1,(1,1)), (2,(1,3)), (3,(3,3)), (4,(3,1)), (5,(1,1))]
-- Filtra los índices con desigualdad => [2,4]
--}

{--
b) oblongoNumber::[Int] genera la lista de los números oblongos. 
Un número es oblongo si es el producto de dos naturales consecutivos.
Por ejemplo, los números [ 2, 6, 12, 20, ...]
--}
oblongoNumber::[Int]
oblongoNumber = [x * (x+1) | x <- [1..10]]

{--
c) abundantes::[Integer] que es la lista de todos los números abundantes. 
Un número natural n se denomina abundante si es menor que la suma de sus divisores propios. 
Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son. 
Por ejemplo abundates = [12, 18, 20, 24, 30, 36, ...
--}
divisores::Integer -> [Integer]
divisores n = [x | x <- [1..n `div` 2], n `mod` x == 0]

sumaDivisores::Integer -> Integer
sumaDivisores n = sum (divisores n)

abundantes::[Integer]
abundantes = [x | x <- [1..30], x <  sumaDivisores x]

{--
d) está repetido.

e) euler :: Int → Int tal que euler n es la suma de todos los múltiplos de 3 o 5 menores que n.
Por ejemplo, euler 10 = 23. 
Puedes usar sin definir la funcion sum que suma los elementos de una lista.
--}
multiplos3o5::Int -> [Int]
multiplos3o5 n = [x | x <- [1..n-1], x `mod`3 == 0 || x `mod` 5 == 0]

euler::Int -> Int
euler n = sum [x | x <- (multiplos3o5 n)] 

{-- 
f) expandir::[Int] -> [Int] 
que reemplace en una lista de números positivos cada número n por n copias de sí mismo:

Ejemplo: expandir [3, 4, 2] = [3, 3, 3, 4, 4, 4, 4, 2, 2]
--}
expandir::[Int] -> [Int]
expandir [] = []
expandir xs = [x | x <- xs, _ <- [1..x]] -- comprension anidada
