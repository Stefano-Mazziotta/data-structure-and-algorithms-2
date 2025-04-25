{--
2) Data  BST a  =  E  |   N  (BST a) a (BST a)
    Type msnTree = BST (Indice,Dato)
a) Suponga que los paquetes llegan (1,“Hola”), (s,”1” ),(6,”!”), (3,”Mundo”) ,(2,” “) ,(4,” “)  Escribir la 
    representacion del mensaje en el tipo  msnTree.
b) Definir mensaje msnTree , retorna Info completa que resulta de pegar datos en los paquetes en 
    orden. Ej: mensaje[(1,”Hola”),(3,”Mundo”),(2,” “)] = “Hola mundo”
--}

data BST a = E | N (BST a) a (BST a) deriving Show
type MsnTree = BST (Indice, Dato)

insertar :: (Indice, Dato) -> MsnTree -> MsnTree
insertar x E = N E x E
insertar x@(ix, _) (N izq y@(iy, _) der)
  | ix < iy   = N (insertar x izq) y der
  | otherwise = N izq y (insertar x der)

msnTreeEjemplo :: MsnTree
msnTreeEjemplo = foldr insertar E [(1,"Hola"), (5,"1"), (6,"!"), (3,"Mundo"), (2," "), (4," ")]

mensaje :: MsnTree -> String
mensaje E = ""
mensaje (N izq (_, dato) der) = mensaje izq ++ dato ++ mensaje der

-- mensaje msnTreeEjemplo
-- Output: "Hola Mundo1!" (con los espacios correctos entre palabras)
