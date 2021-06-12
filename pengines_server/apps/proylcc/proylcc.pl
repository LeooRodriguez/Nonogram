:- module(proylcc,
	[  
		put/8,resolverNonograma/5
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







resolverNonograma(PistasFilasTam,PistasColTam,PistasF,PistasCol,GrillaRes):-
    lineaSol(PistasFilasTam,PistasF,FilaRes),
    pertenece(FilaRes,GrillaRes),
    verificarCol(GrillaRes, PistasCol,0,PistasColTam).


posiblesSoluciones(PistaF,TamP,PosiblesSol):-
    findall(L,(length(L,TamP),posiblesCombinacionesL(PistaF,L)),PosiblesSol).


cumplePista2([],0,[]).
cumplePista2([X|Xs],0,Xs):- X == "X"; var(X).
cumplePista2([X|Xs],N,Res):- X = "#", N>0, Z is N-1, cumplePista2(Xs,Z,Res).

%generarListas(Pistas,ListaMovida).
%Dada unas Pistas y un "L" genera las posibles combinaciones de L para las pistas.
posiblesCombinacionesL([],[]).
posiblesCombinacionesL([],[Y|_Ys]):-Y\=="#".
posiblesCombinacionesL([X|Xs],[Y|Ys]):- Y="#", (cumplePista2([Y|Ys],X,ListaResto),posiblesCombinacionesL(Xs,ListaResto)).
posiblesCombinacionesL([X|Xs],[Y|Ys]):-Y\=="#", posiblesCombinacionesL([X|Xs],Ys).

pertenece([],[]).
pertenece([Y|Ys],[X|Xs]):-
    member(X,Y),
    pertenece(Ys,Xs).

verificarCol(_GrillaSol,_PistasC,Indice,ColTam):-
    Indice == ColTam.
verificarCol(GrillaSol,PistasC,Indice,ColTam):-
    transpose(GrillaSol, GrillaSolT), 
    recorrer(GrillaSolT, Indice, ColN),
    transpose(GrillaSolT, GrillaSolNew), 
    nth0(Indice,PistasC,PistasCN),
    cumpleProp(PistasCN,ColN,ColSat),
    ColSat == 1,
    IndiceAux is Indice+1,
    verificarCol(GrillaSolNew,PistasC,IndiceAux,ColTam).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cumpleProp(Pistas,ListaMovida,1):-cumple(ListaMovida, Pistas).
cumpleProp(_Pistas,_ListaMovida,0).

lineaSol(_Tam,[],[]).
lineaSol(Tam,[X|Xs],Sol):-
    posiblesSoluciones(X,Tam,Res),
	append([Res],LineaSol,Sol),
	lineaSol(Tam,Xs,LineaSol).










