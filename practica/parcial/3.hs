-- 3) Dar el tipo de ruda  f g h = f(gh)(gh)

ruda::(b -> b -> c) -> (a -> b) -> a -> c
ruda f g h = f(gh)(gh)