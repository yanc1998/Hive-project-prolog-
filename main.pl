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
            delete_ficha/1,
            cord_midel/1
          ]).


:- (dynamic tablero/5).
:- (dynamic midel_pixel/1).
:- (dynamic num_ficha/1).
:- (dynamic deep_k/2).
:- (dynamic visit/1).
:- (dynamic visit_board/1).
:- (dynamic selected_type/1).
:- (dynamic to_move/1).
:- (dynamic valid_positions/1).
:-(dynamic fichas/1).
:-(dynamic temp_push/3).
:-(dynamic possible_pos/1).



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
cord_midel([0,0]).
%numero de fichas en el tablero,sirve de identificador para las fichas
num_ficha(1).

%paso las cordenadas q y r y me retorna la coredenada s
axial_to_cube(Q, R, S) :-
    S is -Q-R.

delete_ficha(ID) :-
    retract(tablero(ID, _, _, _, _)).



    
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



for(N, N, _, _) :- !.
for(I, N, Ejecuta, Args) :-
    T=..[Ejecuta, I, Args],
    call(T),
    IN is I+1,
    for(IN, N, Ejecuta, Args), !.
for(I, N, Ejecuta, Args) :-
    IN is I+1,
    for(IN, N, Ejecuta, Args).
    





concat([], X, X).
concat([X|R], Y, [X|Z]) :-
    concat(R, Y, Z).



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


%///////////////////////////////////Push///////////////////////////////////////////////////
    
cant_fichasxtype(grasshopper, 3, black).%saltamontes
cant_fichasxtype(soldier_ant, 3, black).%hormigas
cant_fichasxtype(beetle, 2, black).%escarabajos
cant_fichasxtype(spider, 2, black).%arañas
cant_fichasxtype(queen_bee, 1, black).%abeja reina
cant_fichasxtype(mosquito, 1, black).%mosquito
cant_fichasxtype(ladybug, 1, black).%mariquita
cant_fichasxtype(pillbug, 1, black).%bicho bola
cant_fichasxtype(grasshopper, 3, red).%saltamontes
cant_fichasxtype(soldier_ant, 3, red).%hormigas
cant_fichasxtype(beetle, 2, red).%escarabajos
cant_fichasxtype(spider, 2, red).%arañas
cant_fichasxtype(queen_bee, 1, red).%abeja reina
cant_fichasxtype(mosquito, 1, red).%mosquito
cant_fichasxtype(ladybug, 1, red).%mariquita
cant_fichasxtype(pillbug, 1, red).%bicho bola


valid_positions([]).

push_ficha(Type, Color, Q, R):-
    valid_positions(VP),
    isContain((Q,R), VP),
    num_ficha(ID),
    NewID is ID+1,
    assertz(tablero(NewID, Q, R, Type, Color)),
    update_Generic1(ID, NewID, num_ficha),
    update_Generic1(VP,[],valid_positions).

decrement_ficha(Type,Color):-
    cant_fichasxtype(Type,X,Color),
    Y is X-1,
    retract(cant_fichasxtype(Type,_,Color)),
    assertz(cant_fichasxtype(Type,Y,Color)).


%///////////////////////////////////////Select_Push///////////////////////////////////////////////////////

select_ficha(Type,Color,1):-
    push_ficha(Type, Color, 0, 0),!.


select_ficha(_,_,2):-
    valid_positions(P),
    update_Generic1(P,[],valid_positions),
    findall(_, select_ficha_temp1(), _),!.

select_ficha(_,Color,_):-
    valid_positions(P),
    update_Generic1(P,[],valid_positions),
    possible_pos(V),
    update_Generic1(V,[],possible_pos),
    findall(_, select_ficha_temp2(_,Color),_).
    

select_ficha_temp1():-
    numlist(1,6,Ady),
    member(Pos,Ady),
    get_posicion_ady(Pos,Dq,Dr),
    valid_positions(ValidList),
    concat(ValidList,[(Dq,Dr)],V),
    update_Generic1(ValidList,V,valid_positions).



select_ficha_temp2(_,Color):-
    fichas(F),
    update_Generic1(F,[],fichas),
    get_all_position(),
    fichas(Fichas),
    findall(_,select_ficha_temp3(Fichas),_),
    possible_pos(Pp),
    member(Possible,Pp),
    (Q,R) = Possible,
    color_inverse(Color,C),
    not(temp_push(Q,R,C)),
    valid_positions(V),
    not(isContain((Q,R),V)),
    concat(V,[(Q,R)],NV),
    update_Generic1(V,NV,valid_positions).
    


possible_pos([]).

select_ficha_temp3(Fichas):-
    member(F,Fichas),
    (_,Q,R,_,C) = F,
    numlist(1,6,Ady),
    member(Pos,Ady),
    get_posicion_ady(Pos,Dq,Dr),
    Nq is Q + Dq,
    Nr is R + Dr,
    not(tablero(_,Nq,Nr,_,_)),
    possible_pos(VP),
    concat(VP,[(Nq,Nr)],NVp),
    update_Generic1(VP,NVp,possible_pos),
    assertz(temp_push(Nq,Nr,C)).

    
color_inverse(black,red).
color_inverse(red,black).
    

fichas([]).
position():-
    
    tablero(ID,Q,R,T,C),
    fichas(F),
    concat(F,[(ID,Q,R,T,C)],Nf),
    update_Generic1(F,Nf,fichas).


get_all_position():-
    fichas(F),
    update_Generic1(F,[],fichas), 
    findall(_,position,_).   
   
%/////////////////////////////////Select_Mov//////////////////////////////////////////////////////////





