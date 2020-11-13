{-
- Logica Conmputacional 2020-2 
- Unificacion Martelli-Montanari.
- Profesor: Favio Ezequiel Miranda Perea
- Ayudante: Alejandra Krystel Coloapa Díaz
- Laboratorio: Pedro Juan Salvador Sanchez Perez
-}

module AlgMM where

import LPO
import Data.List

-- / funcion que dada una sustitucion, elimina los pares con componentes iguales correspondientes de la forma x := x 
--simpSus s = error "Funcion a implementar"
simpSus :: Subst -> Subst 
simpSus l1 = [y | y <- l1, nosonIguales y]

-- / funcion que dadas 2 sustituciones devuelve su composicion
--compSus s1 s2 = error "Funcion a implementar"
compSus :: Subst -> Subst -> Subst 
compSus s1 s2 = [compSusAux (x) (s2) | x <- s1, ((V (fst(x)) ) /= snd(compSusAux x s2))] 
  ++ [y | y <- s2, ((compSusAux2 (fst(y)) s1) == False)]

compSusAux :: (Nombre, Term) -> Subst -> (Nombre, Term)
compSusAux (x, y) l1 = (x, apsubT y l1) 

compSusAux2 :: Nombre -> Subst -> Bool
compSusAux2 y [] = False
compSusAux2 y l1 
  | (y == fst(head(l1))) = True
  | otherwise = compSusAux2 y (tail(l1))

-- / funcion que dados 2 terminos dewvuelve una lista de sustituciones tales que 
 -- si t1, t2 no son unificables, la lista es vacia
 -- En caso contrario, la lista contiene como unico elemento al unificador correspondiente
 --unifica t s = error "Funcion a implementar"
unifica :: Term -> Term -> [Subst]
unifica x y = compListSus(valSust(unifAux x y))

--Auxiliar que compara los terminos y nos manda una señal ([[]]) si estos no son unifcables.
unifAux :: Term -> Term -> [Subst]
unifAux (F c []) (F f l1) 
  | c == f = [] 
  | otherwise = [[]]
unifAux (F f l1) (F c []) 
  | c == f = []
  | otherwise = [[]]
unifAux (F c []) (V x) = [[(x, F c [])]]
unifAux (V x) (F c []) = [[(x, F c [])]]
unifAux (V x) (V y) = [[(x, V y)]]
unifAux (F f l1) (V x)
  | (x `elem` listaNom(l1)) = [[]]
  | otherwise = [[(x, F f l1)]]
unifAux  (V x) (F f l1)
  | (x `elem` listaNom(l1)) = [[]]
  | otherwise = [[(x, F f l1)]]
unifAux (F f1 l1) (F f2 l2)
  | (f1 /= f2) = [[]]
  | otherwise = unificaListas l1 l2

--Basicamente cumple la funcion del inciso 4.
funUnif :: [Term] -> [Term] -> [Subst]
funUnif [] [] = []
funUnif l1 l2 = simpLisSus(unifAux (head l1) (head l2))
   ++ funUnif (susListTerm (tail l1) (simpLisSus(unifAux (head l1) (head l2)))) (susListTerm (tail l2 ) (simpLisSus(unifAux (head l1) (head l2))))

--Funcion que le aplica a un conjunto de terminos un conjunto de sustituciones
susListTerm :: [Term] -> [Subst] -> [Term]
susListTerm [] l2 = []
susListTerm l1 [] = l1
susListTerm l1 l2 = susListTerm (susListTermAux l1 (head l2)) (tail(l2)) 

--Auxiliar que aplica a un conjunto de terminos una sola sustitucion.
susListTermAux :: [Term] -> Subst -> [Term]
susListTermAux l1 [] = l1
susListTermAux [] l2 = []
susListTermAux l1 l2 = [ apsubT (head l1) l2 ] ++ susListTermAux (tail l1) l2  

--Elimina pares de la forma x:=x de toda una lista de sustituciones.
simpLisSus :: [Subst] -> [Subst]
simpLisSus [] = []
simpLisSus l1 
  | head l1 == [] = [[]] ++ simpLisSus(tail l1)
  | simpSus(head l1) == [] = [] ++ simpLisSus(tail l1)
  | otherwise = [simpSus(head l1)] ++ simpLisSus(tail l1)

--Lista con todas las variables que aparecen en un conjunto de terminos.
listaNom :: [Term] -> [Nombre]
listaNom [] = []
listaNom l1 = varT(head l1) ++ listaNom(tail l1)

--Auxiliar el cual usamos para verficar si los terminos tienen un umg valido o no se pueden unificar.
--Esto lo hace viendo si la lista de sustituciones tiene una sustitucion vacia.
valSust :: [Subst] -> [Subst]
valSust [] = []
valSust l1
  | ([] `elem` l1) = []
  | otherwise = l1

--Realiza la composicion de todos los elementos de una lista de sustituciones.
compListSus :: [Subst] -> [Subst]
compListSus [] = []
compListSus l 
  | length(l) == 1 = l
  | length(l) == 2 = [compSus (head l) (last l)]
  | otherwise = compListSus( [compSus (head l) (head (tail l))] ++ tail(tail(l)) )

-- / funcion auxiliar de unifica que ayuda a unifica cuando los terminos son funciones
--unificaListas s a = error "Funcion a implementar"
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas l1 l2 = funUnif l1 l2

-- / funcion recursiva que es el caso general del ejercicio anterior, es decir, el unificador para una lista de terminos
--unificaConj s = error "Funcion a implementar"
unificaConj :: [Term] -> [Subst]
unificaConj [] = []
unificaConj l1
  | length(l1) == 1 = []
  | length(l1) == 2 = validaConj(unifica (head l1) (last l1))
  | otherwise = compListSus( unificaConj (susListTermAux (tail l1) (head( validaConj(unifica (head l1) (head (tail l1))) )))           
    ++ validaConj(unifica (head l1) (head (tail l1) )) ) 

--Lanza un error si se tiene que los elementos del conjunto de terminos no son unificables.
validaConj :: [Subst] -> [Subst]
validaConj [] = error ("NO unificables")
validaConj l1 = l1

-- / funcion auxiliar de unificaConj para elminar sustituciones repetidas 
elimTrash :: [Subst] -> [Subst]
elimTrash s = error "Funcion a implementar"

-- / funcion recursiva que unifica dos literales
--unificaLit f g = error "Funcion a implementar"
unificaLit :: Form -> Form -> [Subst]
unificaLit (Pr x l1) (Pr y l2)
  | (x /= y) = error ("No unificables")
  | (unificaListas l1 l2) == [[]] = error ("No unificables")
  | otherwise = unificaListas l1 l2

--Funcion que sirve para sacar el conjunto discordia de dos terminos
conjuntoDiscordeTerm :: Term -> Term -> [Term]
conjuntoDiscordeTerm (V x) (V y)
  | x == y = []
  | otherwise = [V x, V y]
conjuntoDiscordeTerm (V x) (F y l1) = [V x, F y l1]
conjuntoDiscordeTerm (F x l1) (V y) = [F x l1, V y]
conjuntoDiscordeTerm (F x l1) (F y l2)
  | x /= y = [(F x l1), (F y l2)]
  | length l1 /= length l2 = [(F x l1), (F y l2)]
  | otherwise = conjuntoDiscordeAux l1 l2

--Auxiliar el cual se usa para encontrar al conjunto entre los elementos de funciones.
conjuntoDiscordeAux:: [Term] -> [Term] -> [Term]
conjuntoDiscordeAux [] [] = []
conjuntoDiscordeAux l1 l2
  | conjuntoDiscordeTerm (head l1) (head l2) == [] = conjuntoDiscordeAux (tail l1) (tail l2)
  | otherwise = conjuntoDiscordeTerm (head l1) (head l2)

--Funcion que sirve para sacar el conjunto discordia de dos Formulas atomicas.
conjuntoDiscordeForm :: Form -> Form -> Either [Form] [Term]
conjuntoDiscordeForm (Pr x l1) (Pr y l2)
  | x /= y = Left [Pr x l1, Pr y l2]
  | length l1 /= length l2 = Left [Pr x l1, Pr y l2]
  | otherwise = Right (conjuntoDiscordeAux l1 l2)

--Definimos una type-class para poder utilizar una funcion con diferente tipo de entradas, en este caso Terminos y Formulas.
--La unica desventaja que tiene es que tenemos que usar either gracias a que el resultado de un conjunto discordia de uan formula
--puede ser o un termino o una formula.
-- Left se usa para las Formulas y Right para los terminos.
class TermForm a where 
  conjuntoDiscorde :: a -> a -> Either [Form] [Term]

instance TermForm Term where
  conjuntoDiscorde (V x) (V y)
   | x == y = Right []
   | otherwise = Right [V x, V y]
  conjuntoDiscorde (V x) (F y l1) = Right [V x, F y l1]
  conjuntoDiscorde (F x l1) (V y) = Right [F x l1, V y]
  conjuntoDiscorde (F x l1) (F y l2)
   | x /= y = Right [(F x l1), (F y l2)]
   | length l1 /= length l2 = Right [(F x l1), (F y l2)]
   | otherwise = Right (conjuntoDiscordeAux l1 l2)


instance TermForm Form where
  conjuntoDiscorde (Pr x l1) (Pr y l2)
      | x /= y = Left [Pr x l1, Pr y l2]
      | length l1 /= length l2 = Left [Pr x l1, Pr y l2]
     | otherwise = Right (conjuntoDiscordeAux l1 l2)	



--Ejemplos
t1 = F "h"[F "f"[V "w"], F "f"[V "w"]]
t2 = F "h"[F "f"[V "w"], F "f"[V "x"]]
t3 = F "h"[V "z", V "z"]
t4 = V "x"
t5 = (F "P" [V "x", F "f" [V "y"]])
t6 = (F "P" [F "g" [V "y", F "a" []], F "f" [F "b" [] ] ])
t7 = (F "P" [F "g" [F "b" [], V "z"], V "w"])


phi = Pr "P"[F "f"[V "z"], V "x"]
gamma = F "f"[V "x"]
theta = Pr "P"[V "x", F "g"[V "z"]]
alpha = Pr"Q"[V "x", F "g"[V "z"]]
beta = F "f"[V "x", F "g"[V "z"]]