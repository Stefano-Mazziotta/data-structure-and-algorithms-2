{--
data Aexp = Num Int | Prod Aexp Aexp |Div Aexp Aexp

4. Dado el siguiente tipo algebraico:
a) Defina un evaluador eval ::Aexp -> Int . ¿Cómo maneja los errores de división por 0?
b) Defina un evaluador seval :: Aexp →Maybe Int 
--}
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

eval::Aexp -> Int
eval (Num x) = x
eval (Prod (Num x) (Num y)) = x * y
eval (Div (Num x) (Num y)) = x `div` y

seval::Aexp -> Maybe Int
seval (Div (Num x) (Num 0)) = Nothing
seval (Num x) = Just x
seval (Prod (Num x) (Num y)) = Just (x * y)
seval (Div (Num x) (Num y)) = Just (x `div` y)
