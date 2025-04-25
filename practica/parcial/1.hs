
{--
1) Tipo [(Indice,Dato)] representa un mensaje
    Type  Dato = <completar>
    Type  Indice = <completar>
    Type  Paquete =  <completar>
<completar>  Mensaje =  [Paquete]

--}

type Dato = String
type Indice = Int
type Paquete = (Indice, Dato)
type Mensaje = [Paquete]

{--
a)  UltimPaq  toma  f  y  Mensaje  xs  devuelve Paquete correspondiente en el primero  o  último    
lugar en el orden del mensaje comp.  

dependiendo que funcion  f  se utilice  

Ej:  ultimoPaq ascend [(3,Mundo),(1,Hola),(4,!), (2, “ ”)] = (4, “! ”)
--}
myMsg::Mensaje
myMsg = [(3,"Mundo"),(1,"Hola"),(4,"!"), (2, ":)")]

ultimoPaquete::(Paquete -> Paquete -> Bool) -> Mensaje -> Paquete
ultimoPaquete f [(index, dato)] = (index, dato)
ultimoPaquete f (x:y:xs)  
    | f x y = ultimoPaquete f (x:xs)
    | otherwise = ultimoPaquete f (y:xs)

ascendente:: Paquete -> Paquete -> Bool
ascendente (index1, dato1) (index2, dato2) = if index1 > index2 then True else False


-- Faltantes, solo con listas por comprensión, dado Mensaje  xs  ordenado ascendentemente devuelve [ Indice ] de los paquetes faltantes.
-- Faltantes [(1,”Hola”), (3,”Mundo”), (6,”!”] = [2,4,5].  Puede usar concat

faltantes :: Mensaje -> [Indice]
faltantes [(x,y)]       =   [x]
faltantes ((xi,y) : xs)    = [x | x <- [xi..fst (ultimoPaquete ascendente xs)], not (existe x xs) && x /= xi]

existe::Indice -> Mensaje -> Bool
existe i [] = False
existe i ((xi, xd):xs)
    | i == xi = True
    | otherwise = existe i xs 

myMsg2 :: Mensaje
myMsg2 = [(1,"Mundo"),(5,"Hola"),(10,"Hola")]

{--
b) En orden Asc usando foldr y map, dado un mensaje retorna si los paquetes estan ordenados ascendentemente o no.
--}
enOrdenAsc::Mensaje -> Bool
enOrdenAsc xs  = 
    let as = map (\ (i, d) -> i) xs -- retorna Indice 
        ady = zip as (tail as)  -- [(1,2),(2,3)]
    in foldr (\ (t1,t2) acc -> t1 <= t2 && acc) True ady