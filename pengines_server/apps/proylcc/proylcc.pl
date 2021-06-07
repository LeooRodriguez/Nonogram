:- module(proylcc,
	[  
		put/8
	]).

:-use_module(library(lists)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY es el resultado de reemplazar la ocurrencia de X en la posición XIndex de Xs por Y.

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% put(+Contenido, +Pos, +PistasFilas, +PistasColumnas, +Grilla, -GrillaRes, -FilaSat, -ColSat).
%

	% NewGrilla es el resultado de reemplazar la fila Row en la posición RowN de Grilla
	% (RowN-ésima fila de Grilla), por una fila nueva NewRow.
	% NewRow es el resultado de reemplazar la celda Cell en la posición ColN de Row por _,
	% siempre y cuando Cell coincida con Contenido (Cell se instancia en la llamada al replace/5).
	% En caso contrario (;)
	% NewRow es el resultado de reemplazar lo que se que haya (_Cell) en la posición ColN de Row por Conenido.

% put(+Contenido, +Pos, +PistasFilas, +PistasColumnas, +Grilla, -GrillaRes, -FilaSat, -ColSat).
	% Pos es la posicion de la celda seleccionada
	% PistasFilas es la lista de pistas de las filas 
	% PistasColumnas es la lista de pistas de las columnas
	% GrillaRes es la grilla resultante con los nuevis cambios
	% FilaSat = 0 si la fila cumple la propiedad, caso contrario FilaSat = 1
	% ColSat = 0 si la columna cumple la propiedad, caso contrario ColSat = 1	 
	
put(Contenido, [RowN, ColN], PistasFilas, PistasColumnas, Grilla, NewGrilla,FilaSat,ColSat):-
	replace(Row, RowN, NewRow, Grilla, NewGrilla),
	(replace(Cell, ColN, _, Row, NewRow),Cell == Contenido;replace(_Cell, ColN, Contenido, Row, NewRow)),
	recorrer(NewGrilla,RowN,ListaFila),
	recorrer(PistasFilas,RowN,ListFilaPistas),
	cumpleLineaWrapper(ListaFila,ListFilaPistas,FilaSat),
	transpose(NewGrilla,Gtras),
	recorrer(Gtras,ColN,ListaCol),
	recorrer(PistasColumnas,ColN,ListColPistas),
	cumpleLineaWrapper(ListaCol,ListColPistas,ColSat).


transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([] , _ , []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).    


% cumpleLineaWrapper(+Lista,+ListaPistas,-Res).

	% Lista es la lista de la fila o columna de la grilla en donde se necesita verificar si cumple la propiedad
	% ListaPistas es la lista de listas de las pistas ya sea de la fila o columna 
	% Res = 0 si cumple la propiedad, en caso contrario Res = 1

cumpleLineaWrapper(Lista,ListaPistas,0):- not(cumple(Lista,ListaPistas)).
cumpleLineaWrapper(Lista,ListaPistas,1):- cumple(Lista,ListaPistas).

% cumple(+Lista, +Pistas).
	% Lista es la lista de la fila/ columna que queremos recorrer 
	% Pistas es la lista de listas de pistas 
	% En caso de que cumpla la propiedad retorna true, en otro caso retorna false
	% Z es el índice del número de las pistas

cumple([],[]).
cumple([X|Xs],[]):- (X == "X"; var(X)) ,cumple(Xs,[]).
cumple([X|Xs],Pistas):- (X == "X"; var(X)) ,cumple(Xs,Pistas).
cumple([X|Xs],[Y|Ys]):- X == "#", Z is Y-1, cumplePista(Xs,Z,Res), cumple(Res,Ys).

% cumplePista(+[X|Xs],+N,+Res).
	% [X|Xs] es la lista ya sea de la fila o columuna en donde queremos verificar la propiedad
	% N es el indice de la lista de pistas en donde queremos recorrer

cumplePista([],0,[]).
cumplePista([X|Xs],0,Xs):- X == "X"; var(X).
cumplePista([X|Xs],N,Res):- X == "#", Z is N-1, cumplePista(Xs,Z,Res).

% recorrer(+Lista, +Index, +ListaRes).
	% Lista es la lista que deseamos recorrer 
	% ListaRes es la lista que tiene el indice = Index, por lo tanto es la que debo recorrer 

recorrer([X|_Xs],0,X).
recorrer([_X|Xs],Index,ListaRes):- Index>0, I is Index-1, recorrer(Xs,I,ListaRes).


% generarLista(+ListaPistas,-ListaResultado).
	% Genera una lista en base a la lista de pistas.
generarLista([0], []).
generarLista([], Res):-llenarconX(Res).
generarLista([X|Xs], Res):-X == 0, generarLista(Xs, Res). 
generarLista([X|Xs], [Y|Ys]):-Y = "#",
   cumplePista2([Y|Ys], X, Rta),
   Resto = Rta,
   primerHashtag(Resto, LR),
   ListaRestoH = LR,
   generarLista(Xs, ListaRestoH).
generarLista([X|Xs], [Y|Ys]):-Y = "X",
    primerHashtag([Y|Ys], R),
    ListaEnHashtag = R,
    generarLista([X|Xs], ListaEnHashtag).


% llenarconX(+Lista).
	% Completa la lista de entrada con X. 
llenarconX([]).
llenarconX([X|Xs]):- X = "X",llenarconX(Xs).


% primerHashtag(+Lista,-ListaRes).
	% Devuelve el resto de la Lista en ListaRes cuando encuentra un #. 
primerHashtag([], []).
primerHashtag([X|Xs], [X|Xs]):-X = "#".
primerHashtag([X|Xs], Res):-X = "X",primerHashtag(Xs, Res).


% cumplePista2(+Lista,+CantPistas,-ListaRes). 
cumplePista2([],0,[]).
cumplePista2([X|Xs],0,[X|Xs]):- X = "X"; var(X).
cumplePista2([X|Xs],N,Res):- X = "#", Z is N-1, cumplePista2(Xs,Z,Res).