:- module(logic,
          [ tablero/5,
            get_posicion_ady/3,
            update_Generic1/3,
            update_Generic2/4,
            isContain/2,
            concat/3,
            visit_board/1,
            types_bug/1,
            pos_picture_player/2,
            midel_pixel/1,
            add_ficha/5,
            selected_type/1,
            dimention_board/2,
            to_move/1,
            delete_ficha/1
          ]).


:- (dynamic tablero/5).
:- (dynamic midel_pixel/1).
:- (dynamic num_ficha/1).
:- (dynamic deep_k/2).
:- (dynamic visit/1).
:- (dynamic visit_board/1).
:- (dynamic selected_type/1).
:- (dynamic to_move/1).


tablero(1, 0, 0, spider, red).

tablero(2, 0, -1, beetle, black).

tablero(3, 1, -1, grasshopper, red).

tablero(4, 1, 0, ladybug, black).

tablero(5, 0, 1, mosquito, red).

tablero(6, -1, 1, pillbug, black).

tablero(7, -1, 0, queen_bee, red).

tablero(8, 1, -2, soldier_ant, black).

types_bug([spider, beetle, grasshopper, ladybug, mosquito, pillbug, queen_bee, soldier_ant]).

pos_picture_player(spider, 40).
pos_picture_player(beetle, 140).
pos_picture_player(grasshopper, 240).
pos_picture_player(ladybug, 340).
pos_picture_player(mosquito, 440).
pos_picture_player(pillbug, 540).
pos_picture_player(queen_bee, 640).
pos_picture_player(soldier_ant, 740).


selected_type(nan).
to_move(0).

dimention_board(1366, 700).

add_ficha(ID, Q, R, Type, Color) :-
    assertz(tablero(ID, Q, R, Type, Color)).



%direccions
%op: q,r+1,s-1
%down: q,r-1,s+1
%left_op: q-1,r,s+1
%right_op: q+1,r-1,s
%left_dawn:q-1,r+1,s
%right_dawn:q+1,r,s-1 
%tablero(q,r,type,color) ,s = -q-r

%posicion del centro del tablero
midel_pixel([683, 350]).
%numero de fichas en el tablero,sirve de identificador para las fichas
num_ficha(1).

%paso las cordenadas q y r y me retorna la coredenada s
axial_to_cube(Q, R, S) :-
    S is -Q-R.


create_ficha(Q, R, Type, Color) :-
    num_ficha(ID),
    assertz(tablero(ID, Q, R, Type, Color)),
    NewID is ID+1,
    retract(num_ficha(ID)),
    assertz(num_ficha(NewID)).

delete_ficha(ID) :-
    retract(tablero(ID, _, _, _, _)).

move_ficha(ID, AdyID, POS) :-
    tablero(AdyID, Q, R, _, _),
    tablero(ID, _, _, T, C),
    get_posicion_ady(POS, Dq, Dr),
    Nq is Q+Dq,
    Nr is R+Dr,
    not(tablero(_, Nq, Nr, _, _)),
    delete_ficha(ID),
    assertz(tablero(ID, Nq, Nr, T, C)).
    


%up
get_posicion_ady(1, 0, -1).
%up right
get_posicion_ady(2, 1, -1).
%down right
get_posicion_ady(3, 1, 0).
%down
get_posicion_ady(4, 0, 1).
%down left
get_posicion_ady(5, -1, 1).
%up left
get_posicion_ady(6, -1, 0).






%crea un ficha adyacente a la ficha cuyo ID se pasa y esta nueva es adyacente por la cara
%POS que es un numero de 1 a 6 donde este indica cual de las 6 caras del exagono es 
%comenzando a contar por 1 desde la cara de arriba de la izquierda
push_ficha(AdyID, POS, Type, Color) :-
    tablero(AdyID, Q, R, _, C),
    C==Color,
    get_posicion_ady(POS, Dq, Dr),
    Nq is Q+Dq,
    Nr is R+Dr,
    not(tablero(_, Nq, Nr, _, _)),
    create_ficha(Nq, Nr, Type, Color).

for(N, N, _, _) :- !.
for(I, N, Ejecuta, Args) :-
    T=..[Ejecuta, I, Args],
    call(T),
    IN is I+1,
    for(IN, N, Ejecuta, Args), !.
for(I, N, Ejecuta, Args) :-
    IN is I+1,
    for(IN, N, Ejecuta, Args).
    
/*
ttt(I,N):-
    numlist(I, N, P),
    member(X,P),
    algo(X)
*/
testfor :-
    for(1, 4, metfor, []).

metfor(I, _) :-
    print(I).    



create_first_ficha(Type, Color) :-
    create_ficha(0, 0, Type, Color).

concat([], X, X).
concat([X|R], Y, [X|Z]) :-
    concat(R, Y, Z).



bf(ID, MaxDist) :-
    retractall(deep_k(_, _)),
    retractall(visit(_)),
    print("ok"),
    K is 0,
    assertz(deep_k(K, [ID])),
    assertz(visit([ID])),
    K1 is K+1,
    M is MaxDist+1,
    for3(K1, M), !.


for3(N, N) :- !.    
for3(K, N) :-
    assertz(deep_k(K, [])),
    Kk is K-1,
    deep_k(Kk, V),
    %print(V),
    for2(V, K),
    K1 is K+1,
    for3(K1, N).

for3(K, N) :-
    K1 is K+1,
    for3(K1, N).

for2([], _) :- !.
for2([X|R], K) :-
    for1(X, 1, 7, K),
    for2(R, K).
for2([_|R], K) :-
    for2(R, K).
    

for1(_, N, N, _) :-
    print(llego), !.
for1(X, I, N, K) :-
    get_posicion_ady(I, Dq, Dr),
    tablero(X, Q, R, _, _),
    Adyq is Q+Dq,
    Adyr is R+Dr,
    tablero(ID, Adyq, Adyr, _, _),
    visit(Visit),
    print(Visit),
    not(member(ID, Visit)),
    concat(Visit, [ID], V),
    retract(visit(Visit)),
    assertz(visit(V)),
    deep_k(K, L),
    concat(L, [ID], L1),
    retract(deep_k(K, L)),
    assertz(deep_k(K, L1)),
    I1 is I+1,
    for1(X, I1, N, K).

for1(X, I, N, K) :-
    I1 is I+1,
    for1(X, I1, N, K).

visit_board([]).


update_Generic1(V, NV, Predicado) :-
    T1=..[Predicado, V],
    T2=..[Predicado, NV],
    retract(T1),
    assertz(T2).

update_Generic2(V, NV, K, Predicado) :-
    T1=..[Predicado, K, V],
    T2=..[Predicado, K, NV],
    retract(T1),
    assertz(T2).


bellmandFord(ID, MaxDist) :-
    %clean mk
    retractall(deep_k(_, _)),
    retractall(visit(_)),
    %visit node and define deep 0
    assertz(deep_k(0, [ID])),
    assertz(visit([ID])),
    numlist(1, MaxDist, L),
    member(K, L),
    assertz(deep_k(K, [])),
    K1 is K-1,
    deep_k(K1, V),
    member(X, V),
    numlist(1, 6, Ady),
    member(I, Ady),
    get_posicion_ady(I, Dq, Dr),
    tablero(X, Q, R, _, _),
    Nq is Q+Dq,
    Nr is R+Dr,
    tablero(IDAdy, Nq, Nr, _, _),
    visit(Visit),
    not(isContain(IDAdy, Visit)),
    print("perro"),
    concat(Visit, [IDAdy], NewVisit),
    print("gato"),
    retract(visit(Visit)),
    assertz(visit(NewVisit)),
    %update_Generic1(Visit, V, visit),
    deep_k(K, D),
    concat(D, [IDAdy], D1),
    retract(deep_k(K, D)),
    assertz(deep_k(K, D1)).
    %update_Generic2(D, D1, K, deep_k).
isContain(X, [X|_]) :- !.
isContain(X, [_|R]) :-
    isContain(X, R).
    



