1. Se quiere especificar un servicio de mensajería de texto que permite enviar y administrar una cantidad limitada de mensajes que son visibles entre múltiples usuarios. Los mensajes tienen un nombre de usuario único y el contenido del mensaje. Para esto se definió el TAD Call como:

tad Call(N: String, A: Set) where
    import Maybe, Bool, Nat
        beep: Nat -> Call N A
        text: N -> A -> Call N A -> Call N A
        lookfor: N -> Call N A -> Maybe (List A)
        kick: N -> Call N A -> Call N A
        count: Call N A -> Nat
        oldest: Call N A -> Maybe A

beep: dado un límite n inserta un nuevo servicio de mensajería vacío de capacidad n.
text: envía un mensaje, si el Call está lleno se inserta y se elimina el mensaje más viejo.
lookfor: dado el nombre de un usuario retorna una lista con sus mensajes si existen.
kick: dado un usuario lo elimina borrando todas las ocurrencias de sus mensajes.
count: retorna la cantidad actual de mensajes en el Call.
oldest: retorna el mensaje más antiguo si existe.

Dar la especificación algebraica del TAD Call. 
Considere que pueden compararse los nombres de las variables N::String usando ===.

text: N -> A -> Call N A -> Call N A
text username msg (beep n) = (text username msg (beep n))
text username msg call = 
    if (count call) < (getSize call)
        then (text username msg call)
        else (text username msg (removeOldest call))

lookfor: N ->Call N A -> Maybe (List A)
lookfor username (beep n) = Nothing
lookfor username (text username2 msg call) =
    if (username === username2)
        then
            case (lookfor username call) of
                Nothing -> Just [msg]
                Just msgs -> Just (msg:msgs)
        else (lookfor username call)

kick: N -> Call N A -> Call N A
kick username (beep n) = (beep n)
kick username (text username2 msg call) =
    if username === username2
        then (kick username call)
        else (text username2 msg (kick username call))

count: Call N A -> Nat
count (text username msg (beep n)) = 1
count (text username msg call) = 1 + (count call)

// nota: funcion auxiliar para obtener el n de
getSize: Call N A -> Nat
getSize (beep n) = n
getSize (text username msg call) = getSize call

// nota: funciona auxiliar para elimiar el ultimo mensaje
removeOldest: Call N A -> Call N A
removeOldest (text username msg (beep n)) = (beep n)
removeOldest (text username msg call) = (text username msg (removeOldest call))

oldest: Call N A -> Maybe A
oldest (beep n) = Nothing
oldest (text username msg (beep n)) = Just msg
oldest (text username msg call) = oldest call


2. Dado los siguientes tipos de datos:

type Name = String
data Call b = Beep Int | Text Name b (Call b)

ejemplo: call (Text "Luis" "hola" (Text "ana" "hi" (Text "pepe" "chau" (Beep 3))) 

Implementar la función lookfor, oldest y kick especificadas en el TAD Call.

tad: lookfor: N -> Call N A -> Maybe (List A)
lookfor::Name -> Call b -> Maybe [b]

lookfor username (Beep n) = Nothing
lookfor name (Text username2 msg call) =
    if name === username2
        then
            case (lookfor name call) of
                Nothing -> Just [msg]
                Just msgs -> Just msg:msgs
        else
            lookfor username call

tad: oldest: Call N A -> Maybe A 
oldest:: Call b -> Maybe b
oldest (Beep n) = Nothing
oldest (Text username msg (Beep n)) = Just msg
oldest (Text username msg call) = oldest call

tad: kick: N -> Call N A -> Call N A
kick:: Name -> Call b -> Call b
kick _ (Beep n) = (Beep n)
kick username (Text sender msg call)  
    | username == sender = kick username call
    | otherwise = (Text sender msg (kick call)) 

3. Dadas las siguientes definiciones:

subs (1:[2,3]) = 
subs [2,3] ++ map (1:) (subs [2,3]) =
(subs [3] ++ map (2:) (subs [3])) ++ map (1:) (subs [2,3]) =
(subs [] ++ map (3:) (subs []) ++ map (2:) ((subs [] ++ map (3:) (subs []))) ++ map (1:) (subs [2,3]) =
{ [[]] ++ map (3:) [[]] ++ map (2:) ([[]] ++ map (3:) [[]]) } ++ map (1:) (subs [2,3]) =
{ [[]] ++ [[3]] ++ map (2:) ([[]] ++ [[3]]) } ++ map (1:) (subs [2,3]) =
[[], [3]] ++ map (2:) ( [[],[3]] ) ++ map (1:) (subs [2,3]) =
[[], [3]] ++ [[2], [2,3]] ++ map (1:) (subs [2,3]) =
[[], [3], [2], [2,3]] ++ map (1:) (subs [2,3]) =
[[], [3], [2], [2,3]] ++ map (1:) [[], [3], [2], [2,3]]
[[], [3], [2], [2,3]] ++ [[1], [1,3],[1,2], [1,2,3]]
[[], [3], [2], [2,3], [1], [1,3], [1,2], [1,2,3]]

subs [] = [[]]
subs (x:xs) = subs xs ++ map (x:) (subs xs)

Puede suponer válida la propiedad:
map f (xs ++ ys) = map f xs ++ map f ys

Voy a demostrar que "subs (map f xs) = map (map f) (subs xs)" usando
Inducción Estructural sobre el tipo [a] para ∀ l :: [a]

Probar por inducción estructural que:
subs (map f xs) = map (map f) (subs xs)

1. Sea l=[]
= subs (map f [])
= subs [] -> def.1 map
= [[]] -> def.1 subs
= [ map f [] ] -> def.1 map 
= map (map f) [[]] -> def.2 map: map f [a] = [f a]
= map (map f) (subs []) -> def.1 subs

2. Sea l=(x:xs)
= subs (map f (x:xs))
= subs ( (f x) : map f xs) -> def.2 map
= subs (map f xs) ++ map ((f x):) (subs (map f xs)) -> def.2 subs
= map (map f) (subs xs) ++ map ((f x):) (map (map f) (subs xs)) -> HI
= map (map f) (subs xs) ++ map (map f) (map (x:) (subs xs)) -> def map
= map (map f) (subs xs ++ map (x:) (subs xs)) -> distributiva map
= map (map f) (subs x:xs)

map f (x:xs)  = (f x) : map f xs

4. Dada la siguiente recurrencia:

T(1) = 1
T(n) = 3T(floor(n/4)) + n^2 con n > 0

a) Resuelva expandiendo la definición en forma algebraica para encontrar las cotas asintóticas.

Sacamos floor y resuelvo T', para n = 4^k
T'(1) = 1
T'(n) = 3T(n/4) + n^2 con n > 0

    3T(n/4) + n^2 con n > 0
    
    k=1
    3 * [3T(n/4^2) + (n/4)^2 ] + n^2
    3^2 * T(n/4^2) + 3 * (n/4)^2 + n^2
    
    k=2
    3^2 * [3T(n/4^3) + (n/4^2)^2 ] + 3 * (n/4^2) + n^2
    3^3 * T(n/4^3) + 3^2 * (n/4^2)^2 + 3 * (n/4^2) + n^2
    
    k=3
    3^3 * [3T(n/4^4) + (n/4^3)^2 ] + 3^2 * (n/4^2)^2 +  3 * (n/4)^2 + n^2
    3^4 * T(n/4^4) + 3^3 * (n/4^3)^2 + 3^2 * (n/4^2)^2 +  3 * (n/4)^2 + n^2
    
    k veces...
    








b) Es posible confirmar el supuesto anterior usando el teorema maestro. De ser posible explica cómo lo harías y confirmarlo o por el contrario justifica por qué no es posible.
