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
unit (l1, []) = (l1, [])
unit (l1, [[]]) = (l1, [[]])
unit (l1, l2) = nuevaSol (l1, l2) (unitCAux l1 l2)

unitCAux :: Modelo -> Formula -> [Literal]
unitCAux l1 l2 
 | length(listaLit l1 l2) == 0 = []
 | otherwise = [head(listaLit l1 l2)]

listaLit :: Modelo -> Formula -> [Literal]
listaLit l1 l2 = [head(x) | x <- l2, ((length(x) == 1) && not(head(x) `elem` l1) && not((meteNeg(Neg (head(x)))) `elem` l1))]

nuevaSol :: Solucion -> [Literal] -> Solucion
nuevaSol (a1,a2) [] = (a1,a2)
nuevaSol (a1, a2) x = (a1++x, eliList (x) (a2))

eliList elem lista = filter (\e -> e/=elem) lista


--  Seccion de funciones para la regla de eliminacion

elim :: Solucion -> Solucion
elim (a1, a2) = (a1, elimAux (a1) (a2))

elimAux :: Clausula -> Formula -> Formula
elimAux [] (x:xs) = (x:xs)
elimAux z [] = []
elimAux z (x:xs) = if(estaEnClausula (head(z)) (x)) then elimAux (z) (eliList (x) (x:xs)) else [x] ++ elimAux(z) (xs)

estaEnClausula :: Literal -> Clausula -> Bool
estaEnClausula z [] = False
estaEnClausula z (x:xs) = if ((==) (z) (x)) then True else estaEnClausula (z) (xs)

-- Seccion de funciones para la regla de reduccion

red :: Solucion -> Solucion
red (a1, a2) = (a1, redAux (a1) (a2))

redAux :: Clausula -> Formula -> Formula
redAux [] (x:xs) = (x:xs)
redAux c [] = []
redAux c (x:xs) = [elimLitdeClau (head c) (x)] ++ redAux c xs 

elimLitdeClau :: Literal -> Clausula -> Clausula
elimLitdeClau l [] = []
elimLitdeClau (Neg l) (x:xs) = if((==) (l) (x)) then elimLitdeClau (Neg l) (xs) else [x] ++ elimLitdeClau (Neg l) (xs)
elimLitdeClau l (x:xs) = if((==) (Neg l) x) then elimLitdeClau (l) (xs) else [x] ++ elimLitdeClau (l) (xs)

-- Seccion de funciones para la regla de separacion

split :: Solucion -> [Solucion]
split (l1, []) = [(l1, [])]
split (l1, l2) 
  | length(splitCAux l1 l2) == 0 = [(l1, l2)] 
  | success(dpll(l1 ++ splitCAux l1 l2, l2)) == True = [dpll(l1 ++ splitCAux l1 l2, l2)]
  | success(dpll(l1 ++ niega(splitCAux l1 l2) , l2)) == True = [dpll(l1 ++ niega(splitCAux l1 l2) , l2)]
  | otherwise = [(l1 ++ splitCAux l1 l2, [[]])]

niega :: [Literal] -> [Literal]
niega [] = []
niega x = [Neg (head(x))]

splitCAux :: Modelo -> Formula -> [Literal]
splitCAux l1 l2  
  | length(elimLitMod (elimNeg(l1)) (elimNeg(concat(l2)))) == 0 = []
  | otherwise = [head(elimLitMod (elimNeg(l1)) (elimNeg(concat(l2))))]

elimLitMod :: Modelo -> [Literal] -> [Literal]
elimLitMod [] l = l
elimLitMod m [] = []
elimLitMod m l = [x | x <- l, not(x `elem` m)]

elimNeg :: [Literal] -> [Literal]
elimNeg [] = []
elimNeg (x:xs) = [elimNegAux(x)] ++ elimNeg(xs)

elimNegAux :: Literal -> Literal
elimNegAux (Neg x) = x
elimNegAux x = x 

-- Seccion de funciones para la regla de conflicto

conflict :: Solucion -> Bool
conflict (a1, []) = False
conflict (a1,(x:xs)) = if (length (x:xs) == 1 && x == [] ) then True else False

-- Seccion de funciones para la regla de exito

success :: Solucion -> Bool
success (a1, [[]]) = False
success (a1, a2) = if(length a2 == 0) then True else False

-- Seccion de las funciones principales de DPLL

dpllsearch :: Solucion -> Solucion
dpllsearch (l1, []) = (l1, [])
dpllsearch (l1, [[]]) = (l1, [[]])
dpllsearch (l1, l2) = elim(head(split(unit(red(head(split(l1, l2)))))))

dpll :: Solucion -> Solucion
dpll (l1, l2)
  | (success(l1, l2) || conflict(l1, l2)) = (l1, l2)
  | length(l1) == 0 = dpllsearch(l1, l2)
  | otherwise = insertaLit ([head(l1)])  (dpll(tail(fst(dpllsearch(l1, l2))), snd(dpllsearch(l1, l2))))

insertaLit :: [Literal] -> Solucion -> Solucion
insertaLit [] (l1, l2) = (l1, l2)
insertaLit x (l1, l2) = (x ++ l1, l2)

main :: Solucion -> Solucion
main (l1, l2)
  | conflict(dpll(l1, l2)) == True = error (escribirError(dpll(l1, l2)))
  | success(dpll(l1, l2)) == True = dpll(l1, l2)

escribirError :: Solucion -> [Char]
escribirError (l1, l2) = "FAIL modelo construido: " ++ show(l1, l2) 


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

ejemplo1 = main ([], exe1)
ejemplo2 = main ([], exe2)
ejemplo3 = main ([], exe3)
ejemplo4 = main ([], exe4)
ejemplo5 = main ([], exe5)   
ejemplo6 = main ([], bueno)   
ejemplo7 = main ([], exe6)