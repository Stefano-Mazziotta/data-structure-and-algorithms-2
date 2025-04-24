{--
5. Si un árbol binario es dado como un nodo con dos subárboles idénticos se puede aplicar
la técnica sharing, para que los subárboles sean representados por el mismo árbol. Definir las
siguientes funciones de manera que se puedan compartir la mayor cantidad posible de elementos
de los árboles creados:

a) completo::a → Int → Tree a, 
tal que dado un valor x de tipo a y un entero d , crea un árbol
binario completo de altura d con el valor x en cada nodo.
--}

completo::a -> Int -> Tree a -> Tree a
data Tree a = E | N (Tree a) a (Tree a)

completo e 0 = E
completo e n = N (completo e (n-1) e (completo e (n-1)))

completo e n =
    let t = completo e (n-1)
    in N t e t


{--
b) balanceado::a → Int → Tree a,
tal que dado un valor x de tipo a y un entero n, crea un árbol
binario balanceado de tamaño n, con el valor x en cada nodo.
--}

balanceado:: a -> Int -> Tree a

-- 7 inclusive

-- 8, 9 y 10, ultima teoria

