{--
7. La definición de member dada en teoría (la cual determina si un elemento está en un bst)
realiza en el peor caso 2∗d comparaciones, donde 'd' es la altura del árbol. 

Dar una definición de menber que realice a lo sumo d + 1 comparaciones. 
Para ello definir member en términos de una función auxiliar que tenga como parámetro el elemento candidato, 
el cual puede ser igual al elemento que se desea buscar. 
(por ejemplo, el último elemento para el cual la comparación de a <= b retornó True)
y que chequee que los elementos son iguales sólo cuando llega a una hoja del árbol
--}

data BinaryTree a = Hoja | Nodo (BinaryTree a) a (BinaryTree a) deriving Show

-- Original member function (for reference)
-- Uses up to 2*d comparisons in worst case
memberBST :: Ord a => a -> BinaryTree a -> Bool
memberBST a Hoja = False
memberBST a (Nodo l b r ) 
    | a > b = memberBST a r
    | a < b = memberBST a l
    | otherwise = True

-- Improved member function (at most d+1 comparisons)
menber :: Ord a => a -> BinaryTree a -> Bool
menber x tree = menberAux x tree Nothing
  where
    menberAux :: Ord a => a -> BinaryTree a -> Maybe a -> Bool
    menberAux _ Hoja candidate = maybe False (== x) candidate
    menberAux x (Nodo l y r) candidate
      | x <= y    = menberAux x l (Just y)  -- Only one comparison: x <= y
      | otherwise = menberAux x r candidate  -- x > y

-- Example tree:
--       5
--      / \
--     3   8
--    / \   \
--   1   4   9
exampleTree :: BinaryTree Int
exampleTree = Nodo (Nodo (Nodo Hoja 1 Hoja) 3 (Nodo Hoja 4 Hoja)) 5 (Nodo Hoja 8 (Nodo Hoja 9 Hoja))

-- Testing:
test1 = menber 4 exampleTree  -- Should return True
test2 = menber 7 exampleTree  -- Should return False
test3 = menber 9 exampleTree  -- Should return True