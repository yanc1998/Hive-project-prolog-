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
            cord_midel/1,
            get_all_positions/0,
            valid_positions/1,
            select_ficha/3,
            push_ficha/4,
            decrement_ficha/2,
            mov_valid_ficha/4,
            turn/1,
            push_bee_black/1,
            push_bee_red/1,
            cant_push/2,
            player_win/1,
            ocupate_pillbug/1,
            empty_pillbug/1,
            last_hability_pillbug/1,
            bicho_o_escarabajo/1,
            play_pillbug/1,
            game_over/1
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
:- (dynamic fichas/1).
:- (dynamic temp_push/3).
:- (dynamic possible_pos/1).
:- (dynamic cant_fichasxtype/3).
:- (dynamic beetle_down/4).
:- (dynamic mkdisc/1).
:- (dynamic adya/1).
:- (dynamic turn/1).
:- (dynamic push_bee_black/1).
:- (dynamic push_bee_red/1).
:- (dynamic cant_push/2).
:- (dynamic player_win/1).
:- (dynamic ocupate_pillbug/1).
:- (dynamic empty_pillbug/1).
:- (dynamic last_mov/1).
:- (dynamic last_hability_pillbug/1).
:- (dynamic play_pillbug/1).
:- (dynamic values_mosquito/1).
:- (dynamic bicho_o_escarabajo/1).
:- (dynamic game_over/1).

%tablero(1,0,0,soldier_ant,black).
%tablero(3,0,-1,spider,black).
%tablero(2,0,1,soldier_ant,red).
%tablero(4,0,2,grasshopper,red).
%tablero(6,-1,3,soldier_ant,red).
%tablero(5,-1,0,queen_bee,black).
%tablero(7,-2,0,soldier_ant,black).
%tablero(8,-1,-1,soldier_ant,red).
%tablero(9,-2,1,beetle,black).
%tablero(10,1,-1,soldier_ant,black).
game_over(0).

%numero de fichas en el tablero,sirve de identificador para las fichas
num_ficha(1).

last_mov(0).
last_hability_pillbug(0).
play_pillbug(0).

turn(black).
push_bee_black(0).
push_bee_red(0).
cant_push(black, 0).
cant_push(red, 0).

%tablero(1, 0, 0, spider, red).

%tablero(2, 0, -1, beetle, black).

%tablero(3, 1, -1, grasshopper, red).

%tablero(4, 1, 0, ladybug, black).

%tablero(5, 0, 1, mosquito, red).

%tablero(6, -1, 1, pillbug, black).

%tablero(7, -1, 0, queen_bee, red).

%tablero(8, 1, -2, soldier_ant, black).
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
cord_midel([0, 0]).


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
    concat(Visit, [IDAdy], NewVisit),
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
cant_fichasxtype(pillbug, 1, black).%bicho bola
cant_fichasxtype(mosquito, 1, black).%mosquito
cant_fichasxtype(beetle, 2, black).%escarabajos
cant_fichasxtype(grasshopper, 3, black).%saltamontes
cant_fichasxtype(ladybug, 1, black).%mariquita
cant_fichasxtype(soldier_ant, 3, black).%hormigas
cant_fichasxtype(queen_bee, 1, black).%abeja reina
cant_fichasxtype(spider, 2, black).%ara??as
cant_fichasxtype(mosquito, 1, red).%mosquito
cant_fichasxtype(pillbug, 1, red).%bicho bola
cant_fichasxtype(beetle, 2, red).%escarabajos
cant_fichasxtype(spider, 2, red).%ara??as
cant_fichasxtype(grasshopper, 3, red).%saltamontes
cant_fichasxtype(soldier_ant, 3, red).%hormigas
cant_fichasxtype(queen_bee, 1, red).%abeja reina
cant_fichasxtype(ladybug, 1, red).%mariquita
valid_positions([]).


push_bee(queen_bee, black) :- !,
    push_bee_black(E),
    update_Generic1(E, 1, push_bee_black).


push_bee(queen_bee, red) :- !,
    push_bee_red(E),
    update_Generic1(E, 1, push_bee_red).  

push_bee(_,_):-!.
    

push_ficha(Type, Color, Q, R) :-
    valid_positions(VP),
    isContain((Q, R), VP),
    num_ficha(ID),
    assertz(tablero(ID, Q, R, Type, Color)),
    decrement_ficha(Type,Color),
    push_bee(Type,Color),
    NewID is ID+1,
    turn(C),
    cant_push(C,X),
    NX is X + 1,
    update_Generic2(X,NX,C,cant_push),
    color_inverse(C, NC),
    update_Generic1(C, NC, turn),
    update_Generic1(ID, NewID, num_ficha),
    update_Generic1(VP, [], valid_positions),
    last_hability_pillbug(L),
    update_Generic1(L,0,last_hability_pillbug),!,
    finish().

decrement_ficha(Type, Color) :-
    cant_fichasxtype(Type, X, Color),
    Y is X-1,
    retract(cant_fichasxtype(Type, _, Color)),
    assertz(cant_fichasxtype(Type, Y, Color)).


%///////////////////////////////////////Select_Push///////////////////////////////////////////////////////

select_ficha(Type, Color, 1) :-
    num_ficha(ID),
    assertz(tablero(ID, 0, 0, Type, Color)),
    push_bee(Type,Color),
    NewID is ID+1,
    decrement_ficha(Type, Color),
    turn(C),
    cant_push(C,X),
    NX is X + 1,
    update_Generic2(X,NX,C,cant_push),
    color_inverse(C, NC),
    update_Generic1(C, NC, turn),
    update_Generic1(ID, NewID, num_ficha),!.



select_ficha(_, _, 2) :-
    valid_positions(P),
    update_Generic1(P, [], valid_positions),
    findall(_, select_ficha_temp1(), _), !.

%select_ficha(Type, _, 7) :-
%    queen_bee_black_in_board(X),
%    X\=1,
%    Type\=queen_bee, !.


select_ficha(Type,Color,_):-
    cant_push(Color, Cant),
    valid_positions(P),
    update_Generic1(P,[],valid_positions),
    before_4(Type,Color,Cant),
    cant_fichasxtype(Type,X,Color),
    X > 0,
    possible_pos(V),
    update_Generic1(V,[],possible_pos),
    retractall(temp_push(_,_,_)),
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
    get_all_positions(),
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
    

before_4(Type,Color,3):-
    before_Temp(Color),!,
    Type == queen_bee.
     
before_4(_,_,_):-!.
 
before_Temp(black):-
    push_bee_black(X),
    X\=1.
 
before_Temp(red):-
    push_bee_red(X),
    X\=1.


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


get_all_positions():-
    fichas(F),
    update_Generic1(F,[],fichas), 
    findall(_,position,_).   


cant_esc(C):-
    findall(ID,
    (
    beetle_down(ID,_,_,_)),C).
    



disconect_hive(Q,R):-
    mkdisc(M),
    update_Generic1(M,[],mkdisc),
    tablero(ID,Q,R,Type,Color),
    retract(tablero(ID,_,_,_,_)),
    findall(_,get_Ady_pos(Q,R),_),
    adya([(Q1,R1)|_]),
    findall(_,dfs(Q1,R1),_),
    assertz(tablero(ID,Q,R,Type,Color)),
    mkdisc(Mk),
    num_ficha(N),
    length(Mk, L),
    cant_esc(C),
    length(C,L1),
    Nn is N-2-L1,
    Nn==L.
    



mkdisc([]).
dfs(Q,R):-
    tablero(ID,Q,R,_,_),
    mkdisc(Mk),
    not(isContain(ID,Mk)),
    concat(Mk,[ID],Nmk),
    update_Generic1(Mk,Nmk,mkdisc),
    get_Ady_pos(Q,R),
    adya(Adyacents),
    member(Ady,Adyacents),
    (Nq,Nr) = Ady,
    dfs(Nq,Nr).

adya([]).
get_Ady_pos(Q,R):-
    adya(A1),
    update_Generic1(A1,[],adya),
    numlist(1,6,Ady),
    member(Pos,Ady),
    get_posicion_ady(Pos,Dq,Dr),
    Nq is Q + Dq,
    Nr is R + Dr,
    tablero(_,Nq,Nr,_,_),
    adya(A),
    concat(A,[(Nq,Nr)],Na),
    update_Generic1(A,Na,adya).    



%/////////////////////////////////Select_Mov//////////////////////////////////////////////////////////
%Repetir codigo asquerosamente 3 veces quitando linea  6 y quitando linea 9 and 11
mov_ficha(mosquito,Color,Q,R):-
    to_move(ID),
    beetle_down(ID,_,_,_),!,
    mov_ficha(beetle,Color,Q,R).

mov_ficha(mosquito,Color,Q,R):-
    tablero(_,Q,R,_,_),!,
    bicho_o_escarabajo(X),
    mov_bicho_o_escarabajo(X,mosquito,Color,Q,R).


mov_ficha(beetle,Color,Q,R):-
    !,
    valid_positions(VP),
    isContain((Q,R), VP),

    to_move(IDmove),
    tablero(IDmove,Aq,Ar,Type,_),
    
    mov_ficha_beetle_temp(IDmove,Q,R,Aq,Ar,Type,Color),
    last_mov(L),
    update_Generic1(L,IDmove,last_mov),

    turn(C),
    color_inverse(C,NC),
    update_Generic1(C,NC,turn),
    update_Generic1(VP,[],valid_positions),
    update_Generic1(IDmove, 0 , to_move),
    act_hability_pillbug(),
    play_pillbug(P),
    update_Generic1(P,0,play_pillbug),
    finish().


mov_ficha(pillbug,_,Q,R):-
    tablero(ID,Q,R,_,_),!,
    valid_positions(V),
    isContain((Q,R),V),
    to_move(T),
    update_Generic1(T,ID,to_move),
    last_hability_pillbug(X),
    update_Generic1(X,ID,last_hability_pillbug),
    empty_pillbug(E),
    length(E,Le),
    Le \= 0,
    update_Generic1(V,E,valid_positions),
    play_pillbug(P),
    update_Generic1(P,1,play_pillbug).



mov_ficha(Type, Color, Q, R):-
    valid_positions(VP),
    isContain((Q,R), VP),
    to_move(ID),
    retract(tablero(ID,_,_,_,_)),
    assertz(tablero(ID, Q, R, Type, Color)),
    last_mov(L),
    update_Generic1(L,ID,last_mov),
    turn(C),
    color_inverse(C,NC),
    update_Generic1(C,NC,turn),
    update_Generic1(ID, 0 , to_move),
    update_Generic1(VP,[],valid_positions),
    act_hability_pillbug(),
    play_pillbug(P),
    update_Generic1(P,0,play_pillbug),
    finish().


mov_bicho_o_escarabajo(1,mosquito,Color,Q,R):-
    mov_ficha(beetle,Color,Q,R),!.

mov_bicho_o_escarabajo(2,mosquito,Color,Q,R):-
    mov_ficha(pillbug,Color,Q,R),!.



act_hability_pillbug():-
    play_pillbug(P),
    P \= 1,
    last_hability_pillbug(L),
    update_Generic1(L,0,last_hability_pillbug),!.

act_hability_pillbug():-!.



mov_ficha_beetle_temp(IDmove,Q,R,Aq,Ar,Type,Color):-
    beetle_down(IDmove,ID,Type1,Color1),
    retract(tablero(IDmove,_,_,_,_)),
    assertz(tablero(ID,Aq,Ar,Type1,Color1)),

    tablero(IDdown,Q,R,Typedown,Colordown),
    retract(tablero(IDdown,Q,R,Typedown,Colordown)),
    assertz(tablero(IDmove, Q, R, Type, Color)),

    retract(beetle_down(IDmove,_,_,_)),
    assertz(beetle_down(IDmove,IDdown,Typedown,Colordown)),!.



mov_ficha_beetle_temp(IDmove,Q,R,_,_,Type,Color):-
    not(beetle_down(IDmove,_,_,_)),
    retract(tablero(IDmove,_,_,_,_)),
    tablero(IDdown,Q,R,Typedown,Colordown),
    retract(tablero(IDdown,_,_,_,_)),
    assertz(tablero(IDmove, Q, R, Type, Color)),

    assertz(beetle_down(IDmove,IDdown,Typedown,Colordown)),!.

mov_ficha_beetle_temp(IDmove,Q,R,_,_,Type,Color):-
    not(tablero(_,Q,R,_,_)),
    assertz(tablero(IDmove, Q, R, Type, Color)),
    beetle_down(IDmove,_,_,_),
    retract(beetle_down(IDmove,_,_,_)),!.

mov_ficha_beetle_temp(_,_,_,_,_,_,_).


remove_only_pos(Q1,R1):-
    valid_positions(Positions),
    
    Positions2 = Positions,
    update_Generic1(Positions,[],valid_positions),

    member(P,Positions2),
    (Q,R) = P,
    
    get_Ady_pos(Q,R),
    my_delete(Q1,R1,NAdy),
    length(NAdy, L),
    L\=0,
    valid_positions(Vp),
    concat(Vp,[(Q,R)],NVp),
    update_Generic1(Vp,NVp,valid_positions).

my_delete(Q1,R1,L):-
    adya(Ady),
    tablero(ID,Q1,R1,_,_),
    not(beetle_down(ID,_,_,_)),
    delete(Ady,(Q1,R1),L1),
    L = L1,!.

my_delete(_,_,L):-
    adya(Ady),
    L = Ady,!.
    


color_bee_push(black):-
    push_bee_black(C),
    C==1.
color_bee_push(red):-
    push_bee_red(C),
    C==1.

mov_valid_ficha(queen_bee,C,Q,R):-
    valid_positions(P),
    update_Generic1(P,[],valid_positions),
    color_bee_push(C),
    tablero(ID,Q,R,_,_),
    not(last_hability_pillbug(ID)),
    disconect_hive(Q,R),
    findall(_, mov_queen(Q,R), _),
    findall(_,remove_only_pos(Q,R),_),!.

mov_valid_ficha(beetle,C,Q,R):-
    valid_positions(P),
    update_Generic1(P,[],valid_positions),
    color_bee_push(C),
    tablero(ID,Q,R,_,_),
    not(last_hability_pillbug(ID)),
    beetle_down(ID,_,_,_),
    findall(_, mov_beetle(Q,R), _),
    findall(_,remove_only_pos(Q,R),_),!.

mov_valid_ficha(beetle,C,Q,R):-
    valid_positions(P),
    update_Generic1(P,[],valid_positions),
    color_bee_push(C),
    tablero(ID,Q,R,_,_),
    not(last_hability_pillbug(ID)),
    disconect_hive(Q,R),
    findall(_, mov_beetle(Q,R), _),
    findall(_,remove_only_pos(Q,R),_),!.

mov_valid_ficha(grasshopper,C,Q,R):-
    valid_positions(P),
    update_Generic1(P,[],valid_positions),
    color_bee_push(C),
    tablero(ID,Q,R,_,_),
    not(last_hability_pillbug(ID)),
    disconect_hive(Q,R),
    findall(_, mov_grasshopper(Q,R), _),
    findall(_,remove_only_pos(Q,R),_),!.


mov_valid_ficha(ladybug,C,Q,R):-
    valid_positions(P),
    update_Generic1(P,[],valid_positions),
    color_bee_push(C),
    tablero(ID,Q,R,_,_),
    not(last_hability_pillbug(ID)),
    disconect_hive(Q,R),
    findall(_, mov_ladybug(Q,R), _),
    findall(_,remove_only_pos(Q,R),_),!.

mov_valid_ficha(soldier_ant,C,Q,R):-
    valid_positions(P),
    update_Generic1(P,[],valid_positions),
    color_bee_push(C),
    tablero(ID,Q,R,_,_),
    not(last_hability_pillbug(ID)),
    disconect_hive(Q,R),
    findall(_, mov_soldier_ant(Q,R), _),!.
    %findall(_,remove_only_pos(Q,R),_),!.

mov_valid_ficha(spider,C,Q,R):-
    valid_positions(P),
    update_Generic1(P,[],valid_positions),
    color_bee_push(C),
    tablero(ID,Q,R,_,_),
    not(last_hability_pillbug(ID)),
    disconect_hive(Q,R),
    findall(_, move_spider(Q,R), _),!.
    %findall(_,remove_only_pos(Q,R),_),!.



mov_valid_ficha(pillbug,C,Q,R):-
    valid_positions(P),
    update_Generic1(P,[],valid_positions),
    empty_pillbug(E),
    update_Generic1(E,[],empty_pillbug),
    ocupate_pillbug(O),
    update_Generic1(O,[],ocupate_pillbug),
    color_bee_push(C),
    findall(_,aux_empty_pillbug(Q,R),_),
    disconect_hive(Q,R),
    findall(_, mov_queen(Q,R),_),
    findall(_,remove_only_pos(Q,R),_),
    findall(_,hability_pillbug(Q,R),_),
    valid_positions(P1),
    ocupate_pillbug(O1),
    concat(P1,O1,P2),
    update_Generic1(P1,P2,valid_positions).

mov_valid_ficha(pillbug,C,Q,R):-
    valid_positions(P),
    update_Generic1(P,[],valid_positions),
    empty_pillbug(E),
    update_Generic1(E,[],empty_pillbug),
    ocupate_pillbug(O),
    update_Generic1(O,[],ocupate_pillbug),
    color_bee_push(C),
    findall(_,aux_empty_pillbug(Q,R),_),
    not(disconect_hive(Q,R)),
    findall(_,hability_pillbug(Q,R),_),
    valid_positions(P1),
    ocupate_pillbug(O1),
    concat(P1,O1,P2),
    update_Generic1(P1,P2,valid_positions).

mov_valid_ficha(mosquito,C,Q,R):-
    tablero(ID,Q,R,_,_),
    not(beetle_down(ID,_,_,_)),!,
    findall(_,mov_mosquito(C,Q,R),_),
    values_mosquito(VM),
    valid_positions(Vp),
    update_Generic1(Vp,VM,valid_positions).

mov_valid_ficha(mosquito,C,Q,R):-
    mov_valid_ficha(beetle,C,Q,R).


bicho_o_escarabajo(0).
values_mosquito([]).

mov_mosquito(C,Q,R):-
    bicho_o_escarabajo(X),
    update_Generic1(X,0,bicho_o_escarabajo),
    values_mosquito(V),
    update_Generic1(V,[],values_mosquito),
    findall(_,get_Ady_pos(Q,R),_),
    adya(Ady),
   
    member(A,Ady),
    (Nq,Nr) = A,
    tablero(_,Nq,Nr,Type,_),
    Type\=mosquito,
    bicho_escarabajo(Type),
    mov_valid_ficha(Type,C,Q,R),
    valid_positions(Valid),
    values_mosquito(M),
    concat(M,Valid,NV),
    update_Generic1(M,NV,values_mosquito).

bicho_escarabajo(pillbug):-
    bicho_o_escarabajo(X),
    X \=1,
    update_Generic1(X,2,bicho_o_escarabajo).

bicho_escarabajo(pillbug):-
    bicho_o_escarabajo(X),
    X ==1,
    update_Generic1(X,2,bicho_o_escarabajo).

bicho_escarabajo(beetle):-
    bicho_o_escarabajo(X),
    X\=2,
    update_Generic1(X,1,bicho_o_escarabajo).

bicho_escarabajo(beetle):-
    bicho_o_escarabajo(X),
    X==2,
    update_Generic1(X,2,bicho_o_escarabajo).

bicho_escarabajo(_):-!.


move_spider(Q,R):-
    tablero(_,Q,R,_,_),
    findall(_,clean_deep_k(),_),
    findall(_,dfs_deep(Q,R,Q,R,[],0),_),
    findall(_,update_valid_spider(),_).

update_valid_spider():-
    deep_k(3,V),
    valid_positions(Positions),
    concat(Positions,V,NPosistions),
    update_Generic1(Positions,NPosistions,valid_positions). 

mov_soldier_ant(Q,R):-
    tablero(_,Q,R,_,_),
    findall(_,clean_deep_k(),_),
    findall(_,dfs_deep(Q,R,Q,R,[],0),_),
    findall(_,update_valid_ant(),_).

update_valid_ant():-

    deep_k(X,V),
    X\=0,
    valid_positions(Positions),
    concat(Positions,V,NPosistions),
    update_Generic1(Positions,NPosistions,valid_positions).    

clean_deep_k():-
    retractall(deep_k(_,_)),
    num_ficha(N),
    numlist(0,N,List),
    member(X,List),
    assertz(deep_k(X,[])).

dfs_deep(Q,R,Qq,Rr,Visit,K):-
    not(isContain((Q,R),Visit)),
    deep_k(K,V),
    not(isContain((Q,R),V)),
    concat(V,[(Q,R)],NV),
    update_Generic2(V,NV,K,deep_k),
    numlist(1,6,Pos),
    member(P,Pos),
    get_posicion_ady(P,Dq,Dr),
    Nq is Q + Dq,
    Nr is R + Dr,
    not(in_between(P,Q,R)),
    not(tablero(_,Nq,Nr,_,_)),
    in_between_number(P,Q,R,N),
    N==1,
    get_Ady_pos(Nq,Nr),
    adya(Ady),
    delete(Ady,(Qq,Rr),Advalid),
    length(Advalid, L),
    L\=0,
    
    
    K1 is K+1,
    
    dfs_deep(Nq,Nr,Qq,Rr,[(Q,R)|Visit],K1).




mov_ladybug(Q,R):-
    tablero(ID,Q,R,_,_),
    bellmandFord(ID,2),
    deep_k(2,Values),
    member(V,Values),
    get_ady(V).

get_ady(ID):-
    tablero(ID,Q,R,_,_),
    numlist(1,6,Ady),
    member(Pos,Ady),
    get_posicion_ady(Pos,Dq,Dr),
    Nq is Q + Dq,
    Nr is R + Dr,
    not(tablero(_,Nq,Nr,_,_)),
    valid_positions(P),
    concat(P,[(Nq,Nr)],Np),
    update_Generic1(P,Np,valid_positions).





mov_grasshopper(Q,R):-
    numlist(1,6,Ady),
    member(Pos,Ady),
    get_posicion_ady(Pos,Dq,Dr),
    Nq is Q + Dq,
    Nr is R + Dr,
    tablero(_,Nq,Nr,_,_),
    mov_line(Nq,Nr,Dq,Dr).

mov_line(Q,R,Dq,Dr):-
    tablero(_,Q,R,_,_),
    Nq is Q + Dq,
    Nr is R + Dr,
    mov_line(Nq,Nr,Dq,Dr),!.

mov_line(Q,R,_,_):-
    valid_positions(P),
    concat(P,[(Q,R)],Np),
    update_Generic1(P,Np,valid_positions).
    
empty_pillbug([]).
ocupate_pillbug([]).

mov_queen(Q,R):-
    numlist(1,6,Ady),
    member(Pos,Ady),
    get_posicion_ady(Pos,Dq,Dr),
    Nq is Q +Dq,
    Nr is R + Dr,
    not(tablero(_,Nq,Nr,_,_)),
    not(in_between(Pos,Q,R)),
    valid_positions(ValidList),
    concat(ValidList,[(Nq,Nr)],V),
    update_Generic1(ValidList,V,valid_positions).

aux_empty_pillbug(Q,R):-
    numlist(1,6,Ady),
    member(Pos,Ady),
    get_posicion_ady(Pos,Dq,Dr),
    Nq is Q +Dq,
    Nr is R + Dr,
    not(tablero(_,Nq,Nr,_,_)),
    empty_pillbug(ValidList),
    concat(ValidList,[(Nq,Nr)],V),
    update_Generic1(ValidList,V,empty_pillbug).

hability_pillbug(Q,R):-
    numlist(1,6,Ady),
    member(Pos,Ady),
    get_posicion_ady(Pos,Dq,Dr),
    Nq is Q +Dq,
    Nr is R + Dr,
    tablero(ID,Nq,Nr,_,_),
    last_mov(Lid),
    Lid \= ID,
    disconect_hive(Nq,Nr),
    not(beetle_down(ID,_,_,_)),
    ocupate_pillbug(ValidList),
    concat(ValidList,[(Nq,Nr)],V),
    update_Generic1(ValidList,V,ocupate_pillbug).


mov_beetle(Q,R):-
    tablero(IDmove,Q,R,_,_),
    numlist(1,6,Ady),
    member(Pos,Ady),
    get_posicion_ady(Pos,Dq,Dr),
    Nq is Q + Dq,
    Nr is R + Dr,
    mov_beetle_temp(Pos,Nq,Nr,Q,R,IDmove).
    



mov_beetle_temp(Pos,Nq,Nr,Q,R,IDmove):-
    not(beetle_down(IDmove,_,_,_)),
    not(tablero(_,Nq,Nr,_,_)),%lina nueva
    not(in_between(Pos,Q,R)),
    valid_positions(ValidList),
    concat(ValidList,[(Nq,Nr)],V),
    update_Generic1(ValidList,V,valid_positions).

%nuevo
mov_beetle_temp(_,Nq,Nr,_,_,IDmove):-
    not(beetle_down(IDmove,_,_,_)),
    tablero(_,Nq,Nr,_,_),
    valid_positions(ValidList),
    concat(ValidList,[(Nq,Nr)],V),
    update_Generic1(ValidList,V,valid_positions).

mov_beetle_temp(_,Nq,Nr,_,_,IDmove):-
    beetle_down(IDmove,_,_,_),
    valid_positions(ValidList),
    concat(ValidList,[(Nq,Nr)],V),
    update_Generic1(ValidList,V,valid_positions).


in_between_number(Pos,Q,R,N):-
    neighbour(Pos,X,Y),
    get_posicion_ady(X,Dq1,Dr1),
    get_posicion_ady(Y,Dq2,Dr2),
    Nq1 is Q + Dq1,
    Nr1 is R + Dr1,
    Nq2 is Q + Dq2,
    Nr2 is R + Dr2,
    in_between_number_temp([Nq1,Nr1],[Nq2,Nr2],N).

in_between_number_temp([Nq1,Nr1],[Nq2,Nr2],N):-
    tablero(_,Nq1,Nr1,_,_),
    tablero(_,Nq2,Nr2,_,_),
    N is 2,!.

in_between_number_temp([Nq1,Nr1],[Nq2,Nr2],N):-
    not(tablero(_,Nq1,Nr1,_,_)),
    not(tablero(_,Nq2,Nr2,_,_)),
    N is 0,!.
in_between_number_temp([_,_],[_,_],N):-
    N is 1.


in_between(Pos,Q,R):-
    neighbour(Pos,X,Y),
    get_posicion_ady(X,Dq1,Dr1),
    get_posicion_ady(Y,Dq2,Dr2),
    Nq1 is Q + Dq1,
    Nr1 is R + Dr1,
    Nq2 is Q + Dq2,
    Nr2 is R + Dr2,
    tablero(_,Nq1,Nr1,_,_),
    tablero(_,Nq2,Nr2,_,_), !.


neighbour(1,6,2):-!.
neighbour(6,5,1):-!.
neighbour(Pos,X,Y):-
    X is Pos - 1,
    Y is Pos + 1.   


player_win(-1).

finish():-
    win(black),
    player_win(X),
    update_Generic1(X, 1, player_win),
    win(red),
    player_win(Y),
    update_Generic1(Y, 0, player_win),!.

finish():-
    win(red),
    player_win(X),
    update_Generic1(X,2,player_win).

finish():-!.


win(Color):-
    color_inverse(Color,IColor),
    tablero(_,Q,R,queen_bee,IColor),
    findall(_,get_Ady_pos(Q,R),_),
    adya(Ady),
    length(Ady,L),
    L == 6.
