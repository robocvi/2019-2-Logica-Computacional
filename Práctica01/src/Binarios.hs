-- | Logica Computacional 2020-02
-- | Practica 1: Introduccion a Haskell
-- | Profesor: Dr. Favio Ezequiel Miranda Perea
-- | Ayudante: Alejandra Krystel Coloapa Díaz
-- | Laboratorio: Pedro Juan Salvador Sánchez Pérez

module Binario where

-- | tipo de dato Binario, es la representacion de todos los numero binarios que empiezan con uno
data Binario = U | Cero Binario | Uno Binario

-- | definicion de la clase Show para el tipo de dato Binario
instance Show Binario where
    show(U) = "1"
    show(Cero a) = show a ++ "0"
    show(Uno b) = show b ++ "1"

-- | sucesor. Regresa el sucesor de un Binario
-- -> ejemplo sucesor de U (uno)  = Cero U , sucesor de 1 es 10
sucesor  :: Binario -> Binario
sucesor U = (Cero U)
sucesor (Cero a) = (Uno (a))
sucesor (Uno b) = (Cero (sucesor b))

-- | suma. Regresa la suma de 2 numeros de un Binarios
-- -> ejemplo suma de U U = Cero U , suma de 1 y 1 es 10
suma :: Binario -> Binario -> Binario
suma U U = (Cero U)
suma U (Cero a) = (Uno a)
suma (Cero a) U = (Uno a)
suma U (Uno a) = (Cero (suma U a))
suma (Uno a) U = (Cero (suma U a))
suma (Cero a) (Cero b) = (Cero (suma a b))
suma (Cero a) (Uno b) = (Uno (suma a b))
suma (Uno a) (Cero b) = (Uno (suma a b))
suma (Uno a) (Uno b) = (Cero (suma U (suma a b)))

-- | producto. Regresa el producto de 2 numeros Binarios
-- -> ejemplo producto de (Cero U) (Cero U) = (Cero (Cero U)) , producto de 10 con 10 es 100
producto :: Binario -> Binario -> Binario
producto U U = U
producto U (Cero a) = (Cero a)
producto (Cero a) U = (Cero a)
producto U (Uno b) = (Uno b)
producto (Uno b) U = (Uno b)
producto (Cero a) (Cero b) = (Cero (Cero (producto a b)))
producto (Cero a) (Uno b) = suma (Cero (producto (Cero a) (b))) (producto a (Uno b))
producto (Uno a) (Cero b) = suma (Cero (producto (Cero a) (b))) (producto a (Uno b))
producto (Uno a) (Uno b) = suma (Uno (producto (Uno a) b)) (producto a (Uno b))

-- La funcion mod toma a un int y lo pasa al modulo indicado en la derecha
-- La funcion div pues divide dos numeros
-- | natBinLista. Regresa el la representacion en Binario de un numero Decimal en forma de Lista
-- -> ejemplo natBinLista 8 = [1,0,0,0]
natBinLista :: Int -> [Int]
natBinLista 0 = []
natBinLista 1 = [1]
natBinLista a = if a `mod` 2 == 0 then natBinLista(a `div` 2) ++ [0]
    else natBinLista (a `div` 2) ++ [1]

-- Se utilizo una funcion "auxiliar" llamada listaABin, su descripcion esta abajo.
-- | sumaBinLista. Regresa la suma de 2 listas que representan 2 numeros binarios.
-- -> ejemplo sumaBinLista de [1] [1,0] = (Uno U)
sumaBinLista :: [Int] -> [Int] -> Binario
sumaBinLista (xs) (ys) = suma (listaAbin xs) (listaAbin ys)

--La funcion toma a un objeto tipo binario y lo pasa a una lista de int.
binALista :: Binario -> [Int]
binALista U = [1]
binALista (Cero a) = (binALista(a) ++ [0])
binALista (Uno b) = (binALista(b) ++ [1])

--La funcion toma una lista de int que representen un binario y lo pasa nuestro tipo binario.
--last es una funcion de listas la cual nos permite tomar al ultimo elemento de una lista.
--init nos permite tomar a la lista sin el ultimo elemento.
listaAbin :: [Int] -> Binario
listaAbin [1] = U
listaAbin (xs)
    | last xs == 0 = (Cero (listaAbin (init xs)))
    | last xs == 1 = (Uno (listaAbin (init xs)))

--natABin: Funcion para punto extra.
natABin :: Int -> Binario
natABin 0 = error "Solo numeros mayores a 0."
natABin 1 = U
natABin a = if a `mod` 2 == 0 then (Cero (natABin(a `div` 2)))
    else (Uno (natABin(a `div` 2)))

--Esta funcion nos permite tomar una lista de int que represente a un binario
--y nos devuelve su representacion decimal.
convierte :: [Int] -> Int
convierte [] = 0
convierte (xs) = ((last xs) + 2 * convierte (init xs))

--binANat: Funcion para punto extra.
binANat :: Binario -> Int
binANat U = 1
binANat a = convierte(binALista(a))

--predecesor: Funcion para punto extra.
predecesor :: Binario -> Binario
predecesor U = U
predecesor a = natABin((binANat a)-1)
