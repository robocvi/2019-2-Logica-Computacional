{-
- Logica Conmputacional 2020-2 
- Practica02 Parte 1, Implementación del algoritmo dpll.
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Alejandra Krystel Coloapa Díaz
- Laboratorio: Pedro Juan Salvador Sanchez Perez
-}

module DPLL where

import LProp
import Data.List

type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)

-- Seccion de funciones para la regla de la clausula unitaria
--unit s@(m,f) = error "Funcion a implementar"

unit :: Solucion -> Solucion
unit (a1, a2) = nuevaSol (a1,a2) (buscaLit(a2))

--Auxiliar que nos permite recorrer la formula para buscar una literal.
buscaLit :: Formula -> Clausula
buscaLit  [] = []
buscaLit (x:xs) = if (length(x) == 1) then x else buscaLit(xs)

--Elimina a la literal de la formula y la incluye en el modelo.
nuevaSol :: Solucion -> Clausula -> Solucion
nuevaSol (a1,a2) [] = (a1,a2)
nuevaSol (a1, a2) x = (x++a1, eliList (x) (a2))

--Elimina a un elemento de una lista.
eliList elem lista = filter (\e -> e/=elem) lista



--  Seccion de funciones para la regla de eliminacion
-- elim s@(m,f) = error "Funcion a implementar"
elim :: Solucion -> Solucion
elim (a1, a2) = (a1, elimAux (a1) (a2))

--  Este auxiliar nos permite eliminar las clausulas que contengan a la primera literal del modelo
elimAux :: Clausula -> Formula -> Formula
elimAux [] (x:xs) = (x:xs)
elimAux z [] = []
elimAux z (x:xs) = if(estaEnClausula (head(z)) (x)) then elimAux (z) (eliList (x) (x:xs)) else [x] ++ elimAux(z) (xs)

-- Este auxiliar nos permite saber si una clausula contiene la literal seleccionada.
estaEnClausula :: Literal -> Clausula -> Bool
estaEnClausula z [] = False
estaEnClausula z (x:xs) = if ((==) (z) (x)) then True else estaEnClausula (z) (xs)

-- Seccion de funciones para la regla de reduccion
-- red s@(m,f) = error "Funcion a implementar"
red :: Solucion -> Solucion
red (a1, a2) = (a1, redAux (a1) (a2))

-- Este auxiliar nos permite recorrer la formula para eliminar la literal seleccionada de las clausulas.
redAux :: Clausula -> Formula -> Formula
redAux [] (x:xs) = (x:xs)
redAux c [] = []
redAux c (x:xs) = [elimLitdeClau (head c) (x)] ++ redAux c xs 

-- Este auxiliar nos ayuda a recorrer la clausula y eliminar la literal deseada.
elimLitdeClau :: Literal -> Clausula -> Clausula
elimLitdeClau l [] = []
elimLitdeClau (Neg l) (x:xs) = if((==) (l) (x)) then elimLitdeClau (Neg l) (xs) else [x] ++ elimLitdeClau (Neg l) (xs)
elimLitdeClau l (x:xs) = if((==) (Neg l) x) then elimLitdeClau (l) (xs) else [x] ++ elimLitdeClau (l) (xs)



-- Seccion de funciones para la regla de separacion

split :: Solucion -> [Solucion]
split (l1, l2) 
  | (==) (splitAux l1 l2) FFalse = [(l1, l2)] 
  | otherwise = [([splitAux l1 l2] ++ l1, l2), ([Neg (splitAux l1 l2)] ++ l1, l2)]

--Nos regresa la literal con mas ocurrencias en la formula para que split tenga sentido.
splitAux :: Modelo -> Formula -> Literal
splitAux m [] = FFalse
splitAux m l1 = fst(maxiLit(ocurrencias(elimLitMod (elimNeg(m)) (elimNeg(concat(l1))) )))

--Crea una lista de ocurrencias en una lista de Literales.
ocurrencias :: [Literal] -> [(Literal, Integer)]
ocurrencias [] = []
ocurrencias (x:xs) = [(x, numDeVeces x (x:xs))] ++ ocurrencias(xs)

--Elimina literales que aparecen en el modelo ya que no queremos hacer split con una literal que ya se encuentre en el modelo.
elimLitMod :: Modelo -> [Literal] -> [Literal]
elimLitMod [] l = l
elimLitMod m [] = []
elimLitMod m l = [x | x <- l, not(x `elem` m)]

--Elimina las negaciones de una lista de literales para que sea mas facil ver cuantas veces aparece una literal en esa lista.
elimNeg :: [Literal] -> [Literal]
elimNeg [] = []
elimNeg (x:xs) = [elimNegAux(x)] ++ elimNeg(xs)

--El metodo que toma literal por literal y les quita la negacion.
elimNegAux :: Literal -> Literal
elimNegAux (Neg x) = x
elimNegAux x = x 

--Nos da el numero de veces que aparece una literal.
numDeVeces :: Eq a => a -> [a] -> Integer
numDeVeces _ [] = 0
numDeVeces x list = sum $ map (\a -> 1) $ filter (== x) list

--Nos devuelve la literal que mas aparece.
maxiLit :: [(Literal, Integer)] -> (Literal, Integer)
maxiLit [] = (FFalse, 0)
maxiLit [(l, x)] = (l, x)
maxiLit (x:xs)
 | snd(maxiLit xs) > snd(x) = maxiLit xs
 | otherwise = x

-- Seccion de funciones para la regla de conflicto
-- conflict (m,f) = error "Funcion a implementar"
conflict :: Solucion -> Bool
conflict (a1, []) = False
conflict (a1,(x:xs)) = if (length (x:xs) == 1 && x == [] ) then True else False

-- Seccion de funciones para la regla de exito

--success (m,f) = error "Funcion a implementar"
success :: Solucion -> Bool
success (a1, [[]]) = False
success (a1, a2) = if(length a2 == 0) then True else False

-- Ejemplos

bueno = [[Neg (V "P"), V "R", Neg (V "T")],[Neg (V "Q"), Neg (V "R")],[V "P",Neg (V "S")],[Neg (V "P"), V "Q", Neg (V "R"), Neg (V "S")]]
exe1 = [[V "p", V "q"],[Neg (V "q")],[Neg (V "p"), V "q", Neg (V "r")]]
exe2 = [[V "p", V "q"],[V "p", Neg (V "q")],[V "r", V "q"],[V "r", Neg (V "q")]]    
exe3 = [[V "p", Neg (V "q")],[Neg (V "p"), V "q"],[V "q", Neg (V "r")],[Neg (V "q"), Neg (V "r")]]
exe4 = [[V "p", V "q"], [V "r", Neg (V "q"), Neg (V "s")], [Neg (V "p"), V "s"], [Neg (V "r")]]
exe5 = [[V "p", V "q", V "r"], 
        [V "p", Neg (V "q"), Neg (V "r")],
        [V "p", Neg (V "w")],
        [Neg (V "q"), Neg (V "r"), Neg (V "w")],
        [Neg (V "p"), Neg (V "q"), V "r"],
        [V "u", Neg (V "x")],
        [V "u", V "x"],
        [V "q", Neg (V "u")],
        [Neg (V "r"), Neg (V "u")]]
exe6 = [[V "p"], [Neg (V "p")]]        
exe7 = Neg (V "p")
