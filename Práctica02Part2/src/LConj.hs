{-
- Logica Conmputacional 2020-2 
- Implementación del Conjunto.
- Profesor: Favio Ezequiel Miranda Perea
- Ayudante: Alejandra Krystel Coloapa Díaz
- Laboratorio: Pedro Juan Salvador Sanchez Perez
-}

module LConj where

-- | equivL. Función que determina si dos conjuntos son equivalentes.
--equivL l1 l2 = error "Funcion a implementar"
equivL :: Eq a => [a] -> [a] -> Bool
equivL [] [] = True
equivL xs ys = subconjunto xs ys && subconjunto ys xs

-- función auxiliar para equivL
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = [x | x <- xs, x `elem` ys] == xs

-- | filtro. Función que dado un predicado y una lista l, devuelve
-- la lista de los elementos de l que cumplen con el predicado.
filtro :: (a -> Bool) -> [a] -> [a]
filtro f [] = []
filtro f l = [x | x <- l, f(x)]

-- | diff. Función que devuelve la diferencia de listas.
diff :: Eq a => [a] -> [a] -> [a]
diff [] [] = []
diff l1 l2 = [x | x <- l1, not(x `elem` l2)]

-- | union. Función que devuelve la union de listas.
union :: Eq a => [a] -> [a] -> [a]
union [] [] = []
union l [] = l
union [] m = m
union l m = [x |x <- l, not(x `elem` m)]++m 

-- / union2. Funcion que devuelve la union de una lista de lista [[1],[2]] = [1,2]
union2 :: Eq a => [[a]] -> [a]
union2 [] = []
union2 l1
 | (length (l1) == 1) == True = head(l1)
 | length (l1) == 2 = (LConj.union (head(l1)) (last(l1)) )
 | otherwise = union2Aux (LConj.union (head(l1)) (last(l1)) ) (init(tail(l1)))


-- / metodo auxiliar que evalua un modelo en un conjunto de proposiciones.
union2Aux :: Eq a => [a] -> [[a]] -> [a]
union2Aux l1 (x:xs)
 | (length(x:xs) == 1) = LConj.union l1 x
 | otherwise = union2Aux (LConj.union l1 x) (xs) 

-- | inter. Función que devuelve la intersección de listas.
inter :: Eq a => [a] -> [a] -> [a]
inter [] [] = []
inter xs ys =
  [x | x <- xs, x `elem` ys]

-- | subconj. Función que devuelve la potencia de una lista
subconj :: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = [x:ps | ps <- subconj xs] ++ subconj xs
