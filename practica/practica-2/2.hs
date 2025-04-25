-- 2. Consideremos un editor de líneas simple. Supongamos que una Línea es una secuencia de
-- caracteres c1, c2, . . . , cnjunto con una posición p, siendo 0 6p 6n, llamada cursor (consideraremos
-- al cursor a la izquierda de un caracter que ser ́a borrado o insertado, es decir como el cursor de la
-- mayoría de los editores). 
-- Se requieren las siguientes operaciones sobre líneas:
-- vacia :: Linea
-- moverIzq :: Linea →Linea
-- moverDer :: Linea →Linea
-- moverIni :: Linea →Linea
-- moverFin :: Linea → Linea
-- insertar :: Char →L ́ınea →L ́ınea
-- borrar :: L ́ınea →L ́ınea
-- La descripci ́on informal es la siguiente: (1) la constante vac ́ıa denota la l ́ınea vac ́ıa, (2) la ope-
-- raci ́on moverIzq mueve el cursor una posici ́on a la izquierda (siempre que ellos sea posible), (3)
-- an ́alogamente para moverDer , (4) moverIni mueve el cursor al comienzo de la l ́ınea, (5) moverFin
-- mueve el cursor al final de la l ́ınea, (6) la operaci ́on borrar elimina el caracterer que se encuentra
-- a la izquierda del cursor, (7) insertar agrega un caracter en el lugar donde se encontraba el cursor
-- y lo mueve una posici ́on a la derecha.
-- Definir un tipo de datos L ́ınea e implementar las operaciones dadas.

type Pointer = Int
-- type String = [Char]
type Line = (String, Pointer)
-- "Ho/la" ("Hola", 2)


empty :: Line
empty = ("",0) 

moveLeft :: Line -> Line
moveLeft (cs,0) = (cs, 0)
moveLeft (cs,p) = (cs, (p - 1))


moveRight :: Line -> Line
-- moveRight (cs, p) | p < length cs =  (cs, (p + 1))
moveRight (cs, p)
    | p < m = (cs, (p+1))
    | otherwise = (cs, m)
    where m = length cs

moveInit :: Line -> Line
moveInit (cs, p) = (cs, 0)

moveEnd :: Line -> Line
moveEnd (cs, p) = (cs, m) where m = length cs

ins c 0 cs = c:cs
ins c p (x:cs) = x:ins c (p-1) cs

insert :: Char -> Line -> Line
insert c (cs, p) = (ins c p cs, (p+1))  
-- $ para concatenar funciones

rm (c:cs) 1 = cs
rm (c:cs) p 
    | p > (length cs + 1) || p < 0 = error "out of range"
    | otherwise = c:rm cs (p-1)

remove:: Line -> Line
remove (cs, 0) = (cs, 0) 
remove (cs, p) = (rm cs p, (p-1))