data Color = R | B
data RBT a = E | N Color (RBT a) a (RBT a)

(!!) (x:xs) 0 = x
(!!) (x:xs) n = (!!) xs (n-1)  

fromList xs = let alt = truncate (logBase 2 (fromIntegral(length xs)))
              in makeBlack (if (even alt) then formList' xs R
                                          else fromList' xs B)

fromList' [] c = E
fromList' xs c = let n = div (length xs) 2 
                     x = xs !! n
                     ant = take n xs
                     pos = drop (n+1) xs
                     c' = if c == R then B else R
                   in N c (fromList' ant c') x (fromList' pos c')