{-
- Logica Conmputacional 2020-2 
- Practica02 Parte 1, Implementación del algoritmo dpll.
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Alejandra Krystel Coloapa Díaz
- Laboratorio: Pedro Juan Salvador Sanchez Perez
-}

module LPS where

import LProp
import LConj

-- | Estado. Tipo que representa un estado de variables.
type Estado = [VarP]

-- | i. Implementación de la función de interpretación.
--i e p = error "Funcion a implementar"
i :: Estado -> Prop -> Bool
i e (V p)
   | p `elem` e = True
   | otherwise = False
i e TTrue = True
i e FFalse = False
i e (Neg(p))
   | i (e) (p) == True = False
   | i (e) (p) == False = True
i e (Conj p q)
   | ((i e p) && (i e q)) == True = True
   | otherwise = False
i e (Disy p q)
   | ((i e p) || (i e q)) == True = True
   | otherwise = False
i e (Imp p q)
   | i e (Disy (Neg(p)) q) == True = True
   | otherwise = False
i e (Equiv p q)
   | i e (Conj (Imp p q) (Imp q p)) == True = True
   | otherwise = False


-- | vars. Función que devuelve el conjunto de variables proposicionales de una
-- fórmula.
vars :: Prop -> [VarP]
vars (V p) = [p]
vars TTrue = []
vars FFalse = []
vars (Neg p) = vars(p)
vars (Conj p q) = LConj.union (vars p) (vars q)
vars (Disy p q) = LConj.union (vars p) (vars q)
vars (Imp p q) = LConj.union (vars p) (vars q)
vars (Equiv p q) = LConj.union (vars p) (vars q)

-- | estados. Función que devuelve todos los posibles estados de una fórmula.
estados :: Prop -> [Estado]
estados p = LConj.subconj(vars p)

-- | modelos. Función que devuelve todos los posibles modelos de una fórmula.
modelos :: Prop -> [Estado]
modelos p = [j | j <- estados(p), (i (j) (p) == True)]

-- | tautologia. Función que indica si una fórmula es una tautología.
tautologia :: Prop -> Bool
tautologia p
   | estados p == modelos p = True
   | otherwise = False

-- | satisfen. Función que determina si una fórmula es satisfacible en un
-- estado dado.
satisfen :: Estado -> Prop -> Bool
satisfen e p = i e p == True

-- | satisf. Función que determina si una fórmula es satisfacible.
satisf :: Prop -> Bool
satisf p = modelos(p) /= []

-- | insatisfen. Función que determina si una fórmula es insatisfacible en un
-- estado dado.
insatisfen :: Estado -> Prop -> Bool
insatisfen e p = i e p == False

-- | satisf. Función que determina si una fórmula es una contradicción.
contrad :: Prop -> Bool
contrad p = modelos p == []

-- | equiv. Función que determina si dos fórmulas son equivalentes.
equiv :: Prop -> Prop -> Bool
equiv p1 p2 = tautologia(Equiv p1 p2)

-- | estadosConj. Función que dado un conjunto de formaulas, devuleva el 
-- conjunto de todos los posibles estados del conjunto de formulas. la funcion Union2 esta en el modulo LConj
--estadosConj lp = error "Funcion a implementar"
estadosConj :: [Prop] -> [Estado]
estadosConj l = subconj(union2 [vars(y) | y <- l])


-- | modelosConj. Función que dado un conjunto de fórmulas, devuelva el 
-- conjunto de todos los posibles modelos del conjunto de fórmulas.
--modelosConj lp = error "Funcion a implementar"
modelosConj :: [Prop] -> [Estado]
modelosConj p = [j | j <- estadosConj(p), (modelosConjAux j p) == True]

modelosConjAux :: Estado -> [Prop] -> Bool
modelosConjAux e [] = True
modelosConjAux e (x:xs)
   | (i e x) == True = modelosConjAux e xs
   | (i e x) == False = False

-- | satisfenConj. Función que dada una interpretación y un conjunto de fórmulas, 
-- indique si el conjunto es satisfacible en el estado dado. 
satisfenConj :: Estado -> [Prop] -> Bool
satisfenConj e lp = (modelosConjAux e lp == True)

-- | satisfConj. Función que dado un conjunto de fórmulas, 
-- indique si el conjunto es satisfacible.
satisfConj :: [Prop] -> Bool
satisfConj l = modelosConj l /= []

-- | insatisfenConj. Función que dada una interpretación y un conjunto de 
-- fórmulas, indique si el conjunto es insatisfacible en el estado dado. 
insatisfenConj :: Estado -> [Prop] -> Bool
insatisfenConj e lp = (modelosConjAux e lp == False)


-- | insatisfConj. Función que dado un conjunto de fórmulas, indique si el 
-- conjunto es insatisfacible.
insatisfConj :: [Prop] -> Bool
insatisfConj l = modelosConj l == []
