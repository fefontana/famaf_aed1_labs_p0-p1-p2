--implementacion de elem o pertenece
pert :: Eq a => a -> [a] -> Bool
pert _ [] = False
pert e (x:xs)
  | e == x    = True
  | otherwise = pert e xs

--length reimplementacion
tam :: [a] -> Int
tam [] = 0
tam (x:xs) = 1 + tam (xs)

--tail reimplementacion
cola :: [a] -> [a]
cola (_:xs) = xs
cola []     = [] --ojo con esta espec

-- < Sum i  : 0<=i<#xs : tam.(xs!i) >
-- suma de los tamaños de cada subsegmento inicial de la lista

sum_tam_sub_segm_i :: [Int] -> Int
sum_tam_sub_segm_i xs
 | xs==[] = 0
-- | xs==[x] = 1
 | otherwise = tam xs + sum_tam_sub_segm_i (cola xs) -- debia sumar el tamanio no uno

-- confusion fue mezclar sintaxis de pattern matching con guardas
