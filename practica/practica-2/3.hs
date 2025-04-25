{--
Lista de doble extremo

Dado el tipo de datos
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a

a) Implementar las operaciones de este tipo algebraico teniendo en cuenta que:
- Las funciones de acceso son headCL, tailCL, isEmptyCL,isCUnit.
- headCL y tailCL no están definidos para una lista vacı́a.
- headCL toma una CList y devuelve el primer elemento de la misma (el de más a la izquierda).
- tailCL toma una CList y devuelve la misma sin el primer elemento.
- isEmptyCL aplicado a una CList devuelve True si la CList es vacı́a (EmptyCL) o False en caso contrario.
- isCUnit aplicado a una CList devuelve True sii la CList tiene un solo elemento (CUnit a) o False en caso contrario.

--}

{--
[] -> emptyCL
['a'] -> CUnit 'a'
['a','b'] -> Consnoc 'a' EmptyCL 'b'
['a', 'b', 'c'] -> Consnoc 'a' (Cunit 'b') 'c'
['a', 'b', 'c', 'd'] -> Consnoc 'a' (Consnoc 'b' EmptyCL 'c' ) 'd'

[[],[1], [1,2]]
Consnoc (EmptyCL CUnit 1 (Consnoc 1 EmptyCL 2))
--}

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

myList = (Consnoc 1 EmptyCL 6)
-- a)
headCL (CUnit x) = x
headCL (Consnoc x xs y) = x

tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x xs y) = Consnoc (headCL xs) (tailCL xs) y

isEmptyCL:: CList a -> Bool
isEmptyCL (EmptyCL) = True
isEmptyCL _ = False

isCUnit:: CList a -> Bool
isCUnit (CUnit x) = True
isCUnit _ = False

-- b Definir una función reverseCL que toma una CList y devuelve su inversa.
-- data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a
reverseCL:: CList a -> CList a
reverseCL (EmptyCL) = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x EmptyCL y) = Consnoc y EmptyCL x
reverseCL (Consnoc x (CUnit y) z) = Consnoc z (CUnit y) x
reverseCL (Consnoc x xs z) = Consnoc z (reverseCL xs) x

-- c) Definir una función inits que toma una CList y devuelve una CList 
-- con todos los posibles inicios de la CList.
-- inits [1,2,3,4]-> 
    
initsCL'::CList a -> CList (CList a)
initsCL' EmptyCL = EmptyCL
initsCL' (CUnit x) =  
initsCL' xs = snoc (initsCL' (popLastCL xs)) xs

{--
d Definir una función lasts que toma una CList y devuelve una CList con todas las posibles terminaciones de la CList.
inits [1,2,3,4]-> [[],[1], [1,2], [1,2,3], [1,2,3,4]]

e) Definir una función concatCL que toma una CList de CList y devuelve la CList con todas ellas concatenadas
--}

-- aux
-- agrega un elemento al inicio de CList
cons:: a -> CList a -> CList a
cons x EmptyCL = CUnit x
cons x (CUnit y) = Consnoc x EmptyCL y
cons x (Consnoc y ys z) = Consnoc x (cons y ys) z

-- agrega un elemento al final de CList
snoc:: CList a -> a -> CList a
snoc EmptyCL x = CUnit x
snoc (CUnit y) x = Consnoc y EmptyCL x
snoc (Consnoc y ys z) x = Consnoc z (snoc ys y) x

popLastCL:: CList a -> CList a
popLastCL EmptyCL = EmptyCL
popLastCL (CUnit x) = EmptyCL
popLastCL (Consnoc x xs y) = cons x xs