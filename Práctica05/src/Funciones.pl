/**
  * Lógica computacional 2020-2
  * Tema : Fundamentos de Prolog.
  * Profesor: Favio Ezequiel Miranda Perea
  * Ayudante: Alejandra Krystel Coloapa Díaz
  * Laboratorio: Pedro Juan Salvador Sanchez Perez
  **/

%% PROGRAMA

 /**
  * @arg nat. Primer parámetro de la exponenciación.
  * @arg nat. Segundo parámetro de la exponenciación.
  * Relación que determina si x**y = z
  **/
expo(_,0,1).
expo(X,Y,Z) :- Y2 is Y-1, expo(X,Y2,Z2), Z is Z2*X.

/**
  * @arg nat. Primer parámetro de la funcion ackerman.
  * @arg nat. Segundo parámetro de la funcion ackerman.
  * Relación que determina si  z es el resultado de
  * aplicarle la funcion Ackerman a los parámetros.
  **/
ackermann(0,Y,Z) :- Z is Y+1.
ackermann(X,0,Z) :- X > 0, X1 is X-1, ackermann(X1, 1, Z).
ackermann(X,Y,Z) :- X > 0, Y > 0, X1 is X-1, Y2 is Y-1, ackermann(X, Y2, Z2), ackermann(X1, Z2, Z).