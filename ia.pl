:- module(ia,
          [ select_best_jugada/1,
            is_play_IA/1
          ]).


:- consult(main),
   import(logic).

:- (dynamic pos_push_IA/1).
:- (dynamic pos_mov_IA/1).
:- (dynamic best_mov/1).
:- (dynamic act_pro/1).
:- (dynamic is_play_IA/1).
:- (dynamic temp_pill/1).

is_play_IA(0).

fichas_posibles(Color, F_mov, F_push) :-
    fichas_posibles_mov(Color, F_mov),
    fichas_posibles_push(Color, F_push).




play(_) :-
    best_mov(Bm),
    [ID, Q, R]=Bm,
    length(Bm, L),
    L\=0,
    integer(ID),
    %es aqui
    logic:tablero(ID, _, _, Type, Color1),
    logic:to_move(IDm),
    logic:update_Generic1(IDm, ID, to_move),
    logic:valid_positions(V),
    logic:update_Generic1(V, [(Q, R)], valid_positions),
    logic:mov_ficha(Type, Color1, Q, R),
    update(Bm, [], best_mov), !.



play(Color) :-
    best_mov(Bm),
    [Type, Q, R]=Bm,
    length(Bm, L),
    L\=0,
    logic:valid_positions(V),
    logic:update_Generic1(V, [(Q, R)], valid_positions),
    logic:push_ficha(Type, Color, Q, R),
    update(Bm, [], best_mov), !.

play(_) :-
    logic:turn(C),
    logic:color_inverse(C, NC),
    logic:update_Generic1(C, NC, turn).




    
    


fichas_posibles_mov(Color, F_mov) :-
    findall(ID,
            logic:tablero(ID, _, _, _, Color),
            F_mov).
    

fichas_posibles_push(Color, F_push) :-
    findall(Type,
            ( logic:cant_fichasxtype(Type, Num, Color),
              Num\=0
            ),
            F_push).


posibles_jugadas(Color, P_mov, P_push) :-
    fichas_posibles(Color, F_mov, F_push),
    findall(_, mov_posibles_jugadas(Color, F_mov), _),
    findall(_,
            push_posibles_jugadas(Color, F_push),
            _),
    pos_mov_IA(P_mov),
    pos_push_IA(P_push).

add_element(Element, V, Values) :-
    findall((Element, Q, R),
            member((Q, R), V),
            Values).



update(V, NV, Predicado) :-
    T1=..[Predicado, V],
    T2=..[Predicado, NV],
    retract(T1),
    assertz(T2).

pos_mov_IA([]).
pos_push_IA([]).

temp_pill([]).
split_valid(pillbug, Values) :- !,
    temp_pill(T),
    update(T, [], temp_pill),
    member(V, Values),
    (ID, Q, R)=V,
    findall(_,
            split_valid_pillbug_temp(ID, Q, R),
            _).

split_valid(mosquito, Values) :-
    temp_pill(T),
    update(T, [], temp_pill),
    logic:bicho_o_escarabajo(2), !,
    member(V, Values),
    (ID, Q, R)=V,
    findall(_,
            split_valid_pillbug_temp(ID, Q, R),
            _).


split_valid(_, Values) :-
    temp_pill(T),
    update(T, Values, temp_pill).



split_valid_pillbug_temp(ID, Q, R) :-
    not(logic:tablero(_, Q, R, _, _)),
    temp_pill(TT),
    logic:concat(TT, [(ID, Q, R)], NTT),
    update(TT, NTT, temp_pill), !.

split_valid_pillbug_temp(ID, Q, R) :-
    logic:tablero(NID, Q, R, _, _),
    tablero(ID,Pq,Pr,_,_),
    get_empty_pos(Pq, Pr, Values),
    member(V, Values),
    (Nq, Nr)=V,
    temp_pill(TT),
    logic:concat(TT, [(NID, Nq, Nr)], NTT),
    update(TT, NTT, temp_pill).

split_valid_pillbug_temp(_, _, _) :- !.
    


get_empty_pos(Q, R, Values) :-
    findall((Nq, Nr),
            ( logic:get_posicion_ady(_, Dq, Dr),
              Nq is Q+Dq,
              Nr is R+Dr,
              not(logic:tablero(_, Nq, Nr, _, _))
            ),
            Values).    


mov_posibles_jugadas(Color, F_mov) :-
    pos_mov_IA(Pm),
    update(Pm, [], pos_mov_IA),
    member(ID, F_mov),
    logic:tablero(ID, Q, R, Type, Color),
    logic:mov_valid_ficha(Type, Color, Q, R),
    logic:valid_positions(V),
    add_element(ID, V, Values),
    findall(_, split_valid(Type, Values), _),
    temp_pill(Tp),
    pos_mov_IA(Pm1),
    logic:concat(Pm1, Tp, NValues),
    update(Pm1, NValues, pos_mov_IA).
    
    

push_posibles_jugadas(Color, F_push) :-
    pos_push_IA(Pp),
    update(Pp, [], pos_push_IA),
    logic:num_ficha(N),
    N\=1,
    member(Type, F_push),
    logic:select_ficha(Type, Color, N),
    logic:valid_positions(V),
    add_element(Type, V, Values),
    pos_push_IA(Pp1),
    logic:concat(Pp1, Values, NValues),
    update(Pp1, NValues, pos_push_IA).

push_posibles_jugadas(_, F_push) :-
    logic:num_ficha(N),
    N==1,
    member(Type, F_push),
    pos_push_IA(Pp),
    logic:concat(Pp, [(Type, 0, 0)], NValues),
    update(Pp, NValues, pos_push_IA).


select_best_jugada(Color) :-
    posibles_jugadas(Color, P_mov, P_push),
    findall(_, select_best_mov(Color, P_mov), _),
    findall(_, select_best_push(Color, P_push), _),
    logic:valid_positions(V),
    play(Color),
    logic:update_Generic1(V, [], valid_positions).

    



select_best_push(Color, P_push) :-
    member((Type, Q, R), P_push),
    logic:valid_positions(V),
    logic:update_Generic1(V, [(Q, R)], valid_positions),
    logic:push_ficha(Type, Color, Q, R),
    cal_pro(Color, Pp1),
    logic:color_inverse(Color, IColor),
    cal_pro(IColor, Pp2),
    logic:tablero(ID, Q, R, _, _),
    act_best_push(Type, Q, R, Pp1, Pp2, Color),
    inverse_push_ficha(Type, Color, ID).



inverse_push_ficha(Type, Color, ID) :-
    logic:turn(T),
    logic:update_Generic1(T, Color, turn),
    retract(tablero(ID, _, _, _, _)),
    logic:num_ficha(N),
    Nn is ID,
    logic:update_Generic1(N, Nn, num_ficha),
    logic:cant_fichasxtype(Type, Cant, Color),
    Y is Cant+1,
    retract(logic:cant_fichasxtype(Type, _, Color)),
    assertz(logic:cant_fichasxtype(Type, Y, Color)),
    logic:cant_push(Color, X),
    X1 is X-1,
    retract(logic:cant_push(Color, _)),
    assertz(logic:cant_push(Color, X1)),
    inv_bee(Type, Color).


inv_bee(queen_bee, black) :- !,
    logic:push_bee_black(E),
    logic:update_Generic1(E, 0, push_bee_black).


inv_bee(queen_bee, red) :- !,
    logic:push_bee_red(E),
    logic:update_Generic1(E, 0, push_bee_red).  

inv_bee(_, _) :- !.




best_mov([]).

act_pro([10, 10, 10]).


select_best_mov(Color, P_mov) :-
    cal_pro(Color, P1),
    logic:color_inverse(Color, IColor),
    cal_pro(IColor, P2),
    P12 is P1-P2,
    act_pro(Ap),
    update(Ap, [P1, P2, P12], act_pro),
    member((ID, Q, R), P_mov),
    logic:valid_positions(V),
    logic:update_Generic1(V, [(Q, R)], valid_positions),
    logic:to_move(IDm),
    logic:update_Generic1(IDm, ID, to_move),
    tablero(ID, Qa, Ra, Type, Color1),
    logic:mov_ficha(Type, Color1, Q, R),
    cal_pro(Color, Pp1),
    logic:color_inverse(Color, IColor),
    cal_pro(IColor, Pp2),
    act_best_mov(ID, Q, R, Pp1, Pp2),
    inverse_mov_ficha(Type, Color1, Qa, Ra, ID).


inverse_mosquito(1, Color, Qa, Ra, ID):-
    inverse_mov_ficha(beetle,Color,Qa,Ra,ID),!.

inverse_mosquito(2,Color, Qa, Ra, ID):-
    logic:turn(T),
    logic:update_Generic1(T, Color, turn),
    retract(tablero(ID, _, _, _, _)),
    assertz(tablero(ID, Qa, Ra, mosquito, Color)),
    logic:player_win(Pw),
    logic:update_Generic1(Pw, -1, player_win),!.

inverse_mosquito(0,Color, Qa, Ra, ID):-
    logic:turn(T),
    logic:update_Generic1(T, Color, turn),
    retract(tablero(ID, _, _, _, _)),
    assertz(tablero(ID, Qa, Ra, mosquito, Color)),
    logic:player_win(Pw),
    logic:update_Generic1(Pw, -1, player_win),!.
    
    
inverse_mov_ficha(mosquito, Color, Qa, Ra, ID) :-
    logic:turn(T),
    logic:update_Generic1(T, Color, turn),
    logic:bicho_o_escarabajo(X),
    inverse_mosquito(X,Color,Qa,Ra,ID).



inverse_mov_ficha(beetle, Color, Qa, Ra, ID) :-
    logic:turn(T),
    logic:update_Generic1(T, Color, turn),
    logic:tablero(ID,_,_,Type,_),
    inverse_mov_beetle(Type, Color, Qa, Ra, ID),
    logic:player_win(Pw),
    logic:update_Generic1(Pw, -1, player_win).




inverse_mov_ficha(Type, Color, Qa, Ra, ID) :-
    logic:turn(T),
    logic:update_Generic1(T, Color, turn),
    retract(tablero(ID, _, _, _, _)),
    assertz(tablero(ID, Qa, Ra, Type, Color)),
    logic:player_win(Pw),
    logic:update_Generic1(Pw, -1, player_win).
    
    



inverse_mov_beetle(Type, Color, Qa, Ra, ID) :-
    logic:beetle_down(ID, IDa, Type1, Color1),
    retract(logic:tablero(ID, Q, R, _, _)),
    assertz(logic:tablero(IDa, Q, R, Type1, Color1)),
    logic:tablero(AId, Qa, Ra, AType, AColor),
    retract(logic:beetle_down(ID, _, _, _)),
    assertz(logic:beetle_down(ID, AId, AType, AColor)),
    retract(logic:tablero(AId, Qa, Ra, AType, AColor)),
    assertz(logic:tablero(ID, Qa, Ra, Type, Color)), !.

inverse_mov_beetle(Type, Color, Qa, Ra, ID) :-
    not(logic:beetle_down(ID, _, _, _)),
    retract(logic:tablero(ID, _, _, _, _)),
    logic:tablero(AId, Qa, Ra, AType, AColor),
    assertz(logic:beetle_down(ID, AId, AType, AColor)),
    retract(logic:tablero(AId, Qa, Ra, AType, AColor)),
    assertz(logic:tablero(ID, Qa, Ra, Type, Color)), !.

inverse_mov_beetle(Type, Color, Qa, Ra, ID) :-
    not(logic:tablero(_, Qa, Ra, _, _)),
    assertz(logic:tablero(ID, Qa, Ra, Type, Color)),
    logic:beetle_down(ID, _, _, _),
    retract(logic:beetle_down(ID, _, _, _)), !.

inverse_mov_beetle(_, _, _, _, _) :- !.


cal_pro(Color, L) :-
    logic:tablero(_, Q, R, queen_bee, Color),
    findall(_, logic:get_Ady_pos(Q, R), _),
    logic:adya(Ady),
    length(Ady, A),
    A\=0,
    L is A, !.

cal_pro(_, L) :-
    L is 1. 


act_best_push(Type, Q, R, Pp1, Pp2, _) :-
    best_mov(Bm),
    act_pro(X),
    [_, Apc, Apic]=X,
    Pp12 is Pp1-Pp2,
    Pp12=<Apic,
    Pp2 =< Apc,
    update(X, [Pp1, Pp2, Pp12], act_pro),
    update(Bm, [Type, Q, R], best_mov),!.


act_best_push(Type, Q, R, Pp1, Pp2, _) :-
    best_mov(Bm),
    act_pro(X),
    Pp12 is Pp1-Pp2,
    length(Bm, L),
    L==0,
    update(X, [Pp1, Pp2, Pp12], act_pro),
    update(Bm, [Type, Q, R], best_mov), !.


act_best_push(_, _, _, _, _, _) :- !.
    

act_best_mov(ID, Q, R, Pp1, Pp2) :-
    best_mov(Bm),
    act_pro(X),
    [Apc, _, Apic]=X,
    Pp12 is Pp1-Pp2,
    Pp12==Apic,
    Pp12>=0,
    Pp1<Apc,
    update(X, [Pp1, Pp2, Pp12], act_pro),
    update(Bm, [ID, Q, R], best_mov), !.


act_best_mov(ID, Q, R, Pp1, Pp2) :-
    best_mov(Bm),
    act_pro(X),
    X=[_, _, Apic],
    Pp12 is Pp1-Pp2,
    Pp12<Apic,
    update(X, [Pp1, Pp2, Pp12], act_pro),
    update(Bm, [ID, Q, R], best_mov), !.

act_best_mov(ID, Q, R, Pp1, Pp2) :-
    best_mov(Bm),
    act_pro(X),
    Pp12 is Pp1-Pp2,
    length(Bm, L),
    L==0,
    update(X, [Pp1, Pp2, Pp12], act_pro),
    update(Bm, [ID, Q, R], best_mov), !.

    
act_best_mov(_, _, _, _, _) :- !.



    

    
    

