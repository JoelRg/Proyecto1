
-- 1.	Usando listas por comprensión.

filterC :: (a -> Bool) -> [a] -> [a]
filterC predicado lista =  [x | x <- lista, predicado x] 


-- 2.	Usando un map.

filterM :: (a -> Bool) -> [a] -> [a]
filterM predicado = concat . map (\x -> if predicado x then [x] else [])


-- 3.	Usando un foldr:

filterF :: (a -> Bool) -> [a] -> [a]
filterF predicado = foldr (\x xs -> if predicado x then (x:xs) else xs ) [] 