{--
1. Definir las siguientes funciones en forma recursiva:
a) borrarUltimo que dada una lista borra el último elemento de la lista. No utilizar reverse, ni tail.
--}
borrarUltimo:: [a] -> [a]
borrarUltimo [] = []
borrarUltimo [_] = []
borrarUltimo (x:xs) = x : borrarUltimo xs

{--
--b) collect :: [(k, v)] → toma un lista de pares (clave, valor) y asocia cada clave ´unica con todos
--los valores con los que estaba apareada originalmente. Por ejemplo: collect
--}
--collect :: 
--collect [] = []
--collect [(k, v)] = [(k, [v])] -- [(1, 2), (2, 3), (1, 4)] -> [ (1, [2, 4]), (2, [3])] 
--acceder a key -> fst 
--collect ((k, v) :xs) = [(k, [v])]

collect :: (Eq k) => [(k, v)] -> [(k, [v])]
collect [] = []
collect ((k, v) : xs) =
    case lookup k resto of
        Just vs -> (k, v : vs) : filtro
        Nothing -> (k, [v]) : resto
  where
    resto = collect xs  -- Llamada recursiva para procesar el resto de la lista
    filtro = filter ((/= k) . fst) resto  -- Elimina la clave duplicada en la parte ya procesada

-- solved by professor
rep 0 e = []
rep n e = e: rep (n-1) e

-- 1era forma
eco' n [] = []
eco' n (x:xs) = rep n x ++ eco' (n+1) xs
eco xs = eco' 1 xs

-- 2da forma
eco' ((e,i):xs) = rep i e ++ eco' xs
eco xs = eco' (zip xs [1..])
eco xs = [ x | (x,i) <- zip xs [1..], y<-[1..i] ]



{--
c) serie que se comporta de la siguiente manera: serie [1, 2, 3] = [[ ], [1], [1, 2], [1, 2, 3]] Dar su
tipo más general.
--}

-- [] -> [[]]
-- [1] -> [[],[1]]
-- [1,2] -> [[], [1], [1,2]]

-- 1. [[],[1],[1,2],[1,2,3]]
-- 2. [[],[1],[1,2]] ++ [[1,2,3]] = [[],[1],[1,2],[1,2,3]]
-- 3. [[],[1]] ++ [[1,2]] = [[],[1],[1,2]]
-- 4. [[],[1]]

serie :: [a] -> [[a]]
serie [] = [[]]
serie [x] = [[],[x]]
serie xs = serie (take ((length xs) - 1) xs) ++ [xs] 


--d) paresIguales :: Int → Int → Int → Int → Bool toma 4 n´umeros enteros y retorna True si de
--dos en dos son iguales (en cualquier orden), en los dem´as casos retorna False. Por ejemplo:
--paresIguales 3 1 1 2 = False paresIguales 3 1 3 1 = True paresIguales 3 3 1 1 = True
--paresIguales 3 1 1 3 = True
paresIguales :: Int -> Int -> Int -> Int -> Bool
paresIguales a b c d 
  | a == b && c == d = True
  | a == c && b == d = True
  | a == d && b == c = True
  | otherwise = False


-- aux
popLast [] = []
popLast [x] = []
popLast (x:xs) = x: popLast xs

-- e) isosceles :: Int → Int → Int → Bool 
-- que dadas la longitud de los lados de un triangulo nos dice si es un triangulo isosceles.

isEqual::Int->Int->Bool
isEqual a b = a == b

isoceles::Int->Int->Int->Bool
isoceles a b c  | isEqual a b && isEqual b c = False 
                | isEqual a b = True
                | isEqual a c = True
                | isEqual b c = True
                | otherwise = False


{--
f) ror: dada una lista xs y un entero n , tal que n <= lenght xs ,
rota los primeros n elementos de xs a la derecha: 
Definir una version recursiva de ror, sin usar drop, take ni tail.
--}

-- splitN 3 [1,2,3,4,5] = ([1,2,3], [4,5])
splitN::Int->[a]->([a],[a])
splitN 0 xs = ([],xs)
splitN _ [] = ([],[])
splitN n (x:xs) = let (ys, zs) = splitN (n-1) xs
                  in (x:ys, zs)

-- ror 3 [ 1, 2, 3, 4, 5] = [4, 5, 1, 2, 3]
ror::Int->[a]->[a]
ror n xs
    | n <= 0 = xs
    | n >= (length xs) = xs
    | otherwise = let (a, b) = splitN n xs
                  in b++a
{-- 

g) upto::Int→Int→[Int] 
Dado dos números enteros n y m devuelve la lista [ n , n + 1, n + 2, ..., m ] en caso que n <= m 
Devolver la lista [] en otro caso.
No usar listas por comprensión.

upto 1 4 = [1,(1 + 1), (1 + 2), 4] = [1,2,3,4]
--}
upto::Int -> Int -> [Int]
upto n m 
  | n > m = []
  | otherwise = n: (upto (n+1) m)

{--
h) eco 
devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tantas
veces como indica su posición. No usar listas por comprensi ́on.
Por ejemplo: eco "hola" = "hoolllaaaa"
--}

-- eco ["h","o", "l", "a"] = ["h","o","o", "l","l","l", "a","a","a","a"]
-- eco ["y","o"] = ["y","o","o"]

eco'::Int -> [Char] -> [Char]
eco' _ [] = []
eco' n (x:xs) = replicate n x ++ eco' (n+1) xs

eco::[Char] -> [Char]
eco xs = eco' 1 xs