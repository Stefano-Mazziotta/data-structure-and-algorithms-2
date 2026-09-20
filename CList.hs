data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

--- a) hacer head, tail, is empty y is unit --

isEmptyCL :: CList a->Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False 

isCUnit :: CList a->Bool
isCUnit EmptyCL = False
isCUnit (CUnit a) = True
isCUnit (Consnoc _ _ _) = False

headCL :: CList a -> a
headCL EmptyCL = error "Clist vacía" 
headCL (CUnit x) = x
headCL (Consnoc x _ _) = x

tailCL :: CList a->CList a 
tailCL EmptyCL = error "Clist vacía"
tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x xy y) = Consnoc (headCL xy) (tailCL xy) y 

--- hasta aca es el a)--- 

--b) hacer reverse -> toma una Clist y devuelve su inversa

reverseCL:: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit a) = (CUnit a)
--reverseCL (Consnoc x EmptyCL y) = Consnoc y EmptyCL x --no es necesaria ya q de ser empty devolvera el caso base
reverseCL (Consnoc x xs y)= Consnoc y (reverseCL xs) x

--- termina b)

--- cons y snoc para agregar elementos a la Clist
--- cons -> al principio de la lista
--- snoc -> al final de la lista 

cons:: a -> CList a -> CList a
cons x EmptyCL = CUnit x
cons x (CUnit y) = Consnoc x EmptyCL y
cons x (Consnoc y ys z) = Consnoc x (cons y ys) z

snoc:: CList a -> a -> CList a 
snoc EmptyCL x = CUnit x
snoc (CUnit y) x = Consnoc y EmptyCL x
snoc (Consnoc x xs y) z = Consnoc x (snoc xs y) z

-- borrar el ultimo elemento de la lista 
borrarUCL:: CList a -> CList a
borrarUCL EmptyCL = EmptyCL
borrarUCL (CUnit x) = EmptyCL
borrarUCL (Consnoc x xs y) = cons x xs

-- borrar el primer elemento de la lista
borrarICL:: CList a -> CList a
borrarICL EmptyCL = EmptyCL
borrarICL (CUnit x) = EmptyCL
borrarICL (Consnoc x xs y) = snoc xs y

---- c) definir funcion inits para Clist -> me devuelve la Clist de los posibles inicios
--- ejemplo: si le mando [1,2,3] -> [[],[1],[1,2],[1,2,3]]

inits :: CList a -> CList (CList a)
inits EmptyCL = EmptyCL -- Vacía da una lista vacía
inits xs = Consnoc EmptyCL (inits (tailCL xs)) (CUnit (headCL xs))


-- d) definir funcion lasts de las CList -> me devuelve la CList de todos los posibles finales de la Clist enviada
-- ejempli: si le mando [1,2,3] -> [[],[3],[2,3],[1,2,3]]

lasts :: CList a -> CList (CList a)
lasts EmptyCL = EmptyCL -- Vacía da vacío
lasts xs = Consnoc xs (lasts (tailCL xs)) (CUnit (headCL xs))

--lastsCL:: CList a -> CList a
--lasts EmptyCL = Cunit EmptyCL

-- e) concatCL 
