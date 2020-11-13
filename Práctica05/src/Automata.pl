/**
  * Lógica computacional 2020-2
  * Tema : Fundamentos de Prolog.
  * Profesor: Favio Ezequiel Miranda Perea
  * Ayudante: Alejandra Krystel Coloapa Díaz
  * Laboratorio: Pedro Juan Salvador Sanchez Perez
  **/

%%PROGRAMA	

%%Estado inicial S=q0
estadoi(q0).


%%Estado final F={q3}
estadof(q3).

/**
  * @arg nat. Estado de partida de la trancisión
  * @arg nat. Símbolo de entrada de la trancisión epsilon.
  * Relación que indica el estado QN del que se llega
  * a partir del estado Q con el simbolo X
  **/
transicion(q0,a,q1).
transicion(q1,c,q2).
transicion(q2,b,q1).
transicion(q2,d,q3).
transicion(q3,e,q0).


/**
  * @arg nat. Lista de simbolos.
  * Relación indica si X es una lista de símbolos aceptados 
  * por el autómata finito determinista.
  **/
afd(X) :- tExt(q0, X).


/**
  * @arg nat. Estado origen de la trancisión
  * @arg nat. Estado siguiente de la trancisión.
  * Relación que determina si a partir del estado Q es posible
  * llegar al estado final (q3) a través de la cadena l.
  **/
tExt(Q, []) :- estadof(Q).
tExt(Q, [X|XS] ) :- transicion(Q, X, Z), tExt(Z, XS).
