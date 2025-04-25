{--
6. Definir las siguientes funciones sobre árboles binarios de búsqueda (BST):
1. maximum :: BinaryTree a -> a, que calcula el máximo valor en un BST.
2. checkBST :: BinaryTree a -> Bool , que chequea si un árbol binario es un BST
--}

data BinaryTree a = Hoja | Nodo (BinaryTree a) a (BinaryTree a) deriving Show

-- binary tree
memberBT :: Eq a => a -> BinaryTree a -> Bool
memberBT a Hoja = False
memberBT a (Nodo l b r ) = a == b || memberBT a l || memberBT a r

inOrder :: BinaryTree a -> [a]
inOrder Hoja = []
inOrder (Nodo l a r ) = inOrder l ++ [a] ++ inOrder r

{--
Con más precisión, un árbol binario de búsqueda (BST) es un árbol binario 't' tal que
    - Si 't' es una Hoja es un BST
    - Si 't' es un Nodo l a r, tanto l como r tienen que ser BST, y se tiene que cumplir que:
        - Si y es una valor en algún nodo de l entonces y <= a
        - Si y es una valor en algún nodo de r entonces a < y
--}

-- binary search tree
memberBST :: Ord a => a -> BinaryTree a -> Bool
memberBST a Hoja = False
memberBST a (Nodo l b r ) 
    | a == b = True
    | a < b = memberBST a l
    | otherwise = memberBST a r

minimumBST :: BinaryTree a -> a
minimumBST (Nodo Hoja a r ) = a
minimumBST (Nodo l a r ) = minimumBST l

-- 1)
maximumBST :: BinaryTree a -> a
maximumBST (Nodo l a Hoja) = a
maximumBST (Nodo l a r) = maximumBST r

-- 2)

-- Check if list is strictly increasing
isIncreasing :: Ord a => [a] -> Bool
isIncreasing [] = True
isIncreasing [_] = True
isIncreasing (x:y:xs) = x < y && isIncreasing (y:xs)

-- Check if binary tree is a valid BST
checkBST :: Ord a => BinaryTree a -> Bool
checkBST tree = isIncreasing (inOrder tree)

-- Example usage:
validBST = Nodo (Nodo Hoja 1 Hoja) 2 (Nodo Hoja 3 Hoja)  -- Valid BST
invalidBST = Nodo (Nodo Hoja 3 Hoja) 2 (Nodo Hoja 1 Hoja)  -- Invalid BST