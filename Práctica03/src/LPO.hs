{-
- Logica Conmputacional 2020-2 
- Sustitucion para LPO.
- Profesor: Favio Ezequiel Miranda Perea
- Ayudante: Alejandra Krystel Coloapa Díaz
- Laboratorio: Pedro Juan Salvador Sanchez Perez
-}
module LPO where

--Modulo de listas brNombreados por el lenguaje
import Data.List

--Tipo que representa el nombre.
type Nombre = String

--Tipo que representa un término.
data Term = V Nombre | F Nombre [Term] deriving(Show,Eq,Ord)

--Tipo que representa una fórmula de LPO.
data Form = TrueF
          | FalseF
          | Pr Nombre [Term]
          | Eq Term Term
          | Neg Form
          | Conj Form Form
          | Disy Form Form
          | Imp Form Form
          | Equi Form Form
          | All Nombre Form
          | Ex Nombre Form deriving(Show,Eq,Ord)

-- | Subst. Tipo que representa una sustitución de variables en términos.
type Subst = [(Nombre,Term)]          

-- | varT. Función que devuelve una lista con todos los índices de variables que
-- figuran en t. (De regalo, les va a servir para apsubF en los casos de All y Ex)
varT :: Term -> [Nombre]
varT (V x) = [x]
varT (F f l) = foldl1 union [varT t | t <- l]

-- | verifSus. Función que verifica una sustitución.
--verifSus s = error "funcion a implementar."
verifSus :: Subst -> Bool
verifSus [] = True
verifSus (x:xs)
  | ((nosonIguales x) && (noRepetidos (fst(x)) xs)) = verifSus xs
  | otherwise = False


nosonIguales :: (Nombre, Term) -> Bool
nosonIguales (x, y)
  | (V x) == y = False
  | otherwise = True


noRepetidos :: Nombre -> Subst -> Bool
noRepetidos y [] = True
noRepetidos y (x:xs)
  | (y == fst(x)) = False
  | otherwise = noRepetidos y xs


-- | apsubT. Función que aplica una sustitución de variables de variables en términos
-- en un término dado.
--apsubT t s = error "funcion a implementar."
apsubT :: Term -> Subst -> Term
apsubT (V x) y = susVar (V x) y
apsubT (F z x) y = F z (susFun x y)

susVar :: Term -> Subst -> Term
susVar (V x) [] = V x
susVar (V x) (y:ys)
  | (x == fst(y)) = snd(y)
  | otherwise = susVar (V x) ys

susFun :: [Term] -> Subst -> [Term]
susFun (x:xs) [] = (x:xs)
susFun [] y = []
susFun (x:xs) y = [apsubT x y] ++ susFun xs y


-- | apsubfuncion que devuelve la sustitucion de terminos en una formula 
--apsubF f s = error "funcion a implementar."
apsubF :: Form -> Subst -> Form
apsubF TrueF y = TrueF
apsubF FalseF y = FalseF
apsubF (Pr z x) y = Pr z (susFun x y)
apsubF (Eq x z) y = Eq (apsubT x y) (apsubT z y)
apsubF (Neg x) y = Neg (apsubF x y)
apsubF (Conj x z) y = Conj (apsubF x y) (apsubF z y)
apsubF (Disy x z) y = Disy (apsubF x y) (apsubF z y)
apsubF (Imp x z) y = Imp (apsubF x y) (apsubF z y)
apsubF (Equi x z) y = Equi (apsubF x y) (apsubF z y)
apsubF (All x z) y 
  | (estaenT x y) = error "Sustitucion no legal"
  | otherwise = All x (apsubF z (nuevaSubst x y))
apsubF (Ex x z) y 
  | (estaenT x y) = error "Sustitucion no legal"
  | otherwise = Ex x (apsubF z (nuevaSubst x y))

estaenT :: Nombre -> Subst -> Bool
estaenT x [] = False
estaenT x (y:ys)
  | ((x) `elem` varT(snd(y))) = True
  | otherwise = estaenT x ys

nuevaSubst :: Nombre -> Subst -> Subst
nuevaSubst x [] = []
nuevaSubst x (y:ys)
  | (x == fst(y)) = nuevaSubst x ys
  | otherwise = [y] ++ nuevaSubst x ys 



