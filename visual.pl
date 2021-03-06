:- use_module(library(pce)).
:- pce_image_directory('./images/').
resource(spider, image, image('spider.jpg')).
resource(beetle, image, image('beetle.jpg')).
resource(grasshopper, image, image('grasshopper.jpg')).
resource(ladybug, image, image('ladybug.jpg')).
resource(mosquito, image, image('mosquito.jpg')).
resource(pillbug, image, image('pillbug.jpg')).
resource(queen_bee, image, image('queen_bee.jpg')).
resource(soldier_ant, image, image('soldier_ant.jpg')).
resource(circlered, image, image('circlered.jpg')).

resource(cero, image, image('0.jpg')).
resource(one, image, image('1.jpg')).
resource(two, image, image('2.jpg')).
resource(thre, image, image('3.jpg')).

resource(winner_one, image, image('winner-1.jpg')).
resource(winner_thwo, image, image('winner-2.jpg')).
resource(empate, image, image('empate.jpg')).


resource(turno1, image, image('turno1.jpg')).
resource(turno2, image, image('turno2.jpg')).


:- consult(main),
   import(logic).

:- consult(ia),
   import(ia).

new_image(Ventana, Figura, Imagen, Posicion) :-
    new(Figura, figure),
    new(Bitmap, bitmap(resource(Imagen), @on)),
    send(Bitmap, name, 1),
    send(Figura, display, Bitmap),
    send(Figura, status, 1),
    send(Ventana, display, Figura, Posicion).


point_hex_corner([X, Y], Lenght, AnguloG, [Xr, Yr]) :-
    AnguloR is 3.141592653589793*AnguloG/180,
    Xr is X+Lenght*cos(AnguloR),
    Yr is Y+Lenght*sin(AnguloR), !.
    

draw_line(W, [X1, Y1], [X2, Y2], Color) :-
    new(H, path),
    send(H, append, point(X1, Y1)),
    send(H, append, point(X2, Y2)),
    send(H, colour, Color),
    send(W, display, H).


new_hexag(W, [X, Y], Lenght, Color) :-
    point_hex_corner([X, Y],
                     Lenght,
                     300,
                     [X1r, Y1r]),
    point_hex_corner([X, Y], Lenght, 0, [X2r, Y2r]),
    point_hex_corner([X, Y],
                     Lenght,
                     60,
                     [X3r, Y3r]),
    point_hex_corner([X, Y],
                     Lenght,
                     120,
                     [X4r, Y4r]),
    point_hex_corner([X, Y],
                     Lenght,
                     180,
                     [X5r, Y5r]),
    point_hex_corner([X, Y],
                     Lenght,
                     240,
                     [X6r, Y6r]),
    draw_line(W, [X1r, Y1r], [X2r, Y2r], Color),
    draw_line(W,
              [X2r, Y2r],
              [X3r, Y3r],
              Color),
    draw_line(W,
              [X3r, Y3r],
              [X4r, Y4r],
              Color),
    draw_line(W,
              [X4r, Y4r],
              [X5r, Y5r],
              Color),
    draw_line(W,
              [X5r, Y5r],
              [X6r, Y6r],
              Color),
    draw_line(W,
              [X6r, Y6r],
              [X1r, Y1r],
              Color).


cord_to_pixel(Q, R, Size, [X, Y]) :-
    logic:midel_pixel([Px, Py]),
    %logic:dimention_board(X1,Y1),
    X is Px+Size*(Q*3/2),
    Y is Py+Size*(Q*sqrt(3)/2+R*sqrt(3)).



pixel_to_cord(X1, Y1, Size, [Q1, R1]) :-
    midel_pixel([X2, Y2]),
    X is X1-X2,
    Y is Y1-Y2,
    Q is 2/3*X/Size,
    R is (-1/3*X+Y*sqrt(3)/3)/Size,
    S is -Q-R,
    Qt is round(Q),
    Rt is round(R),
    St is round(S),
    QD is abs(Qt-Q),
    RD is abs(Rt-R),
    SD is abs(St-S),
    temp_pixel(Qt,
               Rt,
               St,
               QD,
               RD,
               SD,
               [Q1, R1]).
    
temp_pixel(_, R, S, QD, RD, SD, [Q1, R1]) :-
    QD>RD,
    QD>SD,
    Q1 is -R-S,
    R1 is R.

temp_pixel(Q, _, S, _, RD, SD, [Q1, R1]) :-
    RD>SD,
    R1 is -Q-S,
    Q1 is Q.

temp_pixel(Q, R, _, _, _, _, [Q1, R1]) :-
    Q1 is Q,
    R1 is R.    
    



get_midel_pixel(X, Y, Size, 6, Nx, Ny) :-
    H is Size*sqrt(3),
    Nx is X-3*Size/2,
    Ny is Y-H/2.

get_midel_pixel(X, Y, Size, 5, Nx, Ny) :-
    H is Size*sqrt(3),
    Ny is Y+H/2,
    Nx is X-3*Size/2.


get_midel_pixel(X, Y, Size, 4, X, Ny) :-
    H is Size*sqrt(3),
    Ny is Y+H.

get_midel_pixel(X, Y, Size, 3, Nx, Ny) :-
    H is Size*sqrt(3),
    Nx is X+3*Size/2,
    Ny is Y+H/2.

get_midel_pixel(X, Y, Size, 2, Nx, Ny) :-
    H is Size*sqrt(3),
    Ny is Y-H/2,
    Nx is X+3*Size/2.

get_midel_pixel(X, Y, Size, 1, X, Ny) :-
    H is Size*sqrt(3),
    Ny is Y-H.

    

draw_image(Window, Figura, Type, X, Y) :-
    new_image(Window, Figura, Type, point(X, Y)).



draw_hive(Window, Size) :-
    logic:get_all_positions(),
    logic:fichas(Position),
    member(P, Position),
    (ID, Q, R, T, C)=P,
    cord_to_pixel(Q, R, Size, [X, Y]),
    new_hexag(Window, [X, Y], Size, black),
    new_hexag(Window, [X, Y], Size-4, C),
    draw_select_bug(ID, Window, Size-6, X, Y),
    Ix is X-22,
    Iy is Y-22,
    draw_image(Window, _, T, Ix, Iy),
    logic:valid_positions(Valid),
    member(Vp, Valid),
    (Q1, R1)=Vp,
    cord_to_pixel(Q1, R1, Size, [X1, Y1]),
    new_hexag(Window, [X1, Y1], Size, green).



win(1, Window) :-
    dimention_board(Px, _),
    draw_image(Window, _, winner_one, Px-300, 100),
    logic:update_Generic1(0, 1, game_over), !,
    fail.


win(2, Window) :-
    dimention_board(Px, _),
    draw_image(Window, _, winner_thwo, Px-300, 100),
    logic:update_Generic1(0, 1, game_over), !,
    fail.

win(0, Window) :-
    dimention_board(Px, _),
    draw_image(Window, _, empate, Px-300, 100),
    logic:update_Generic1(0, 1, game_over), !,
    fail.

win(_, _).

    

    
draw_board(Window, Size, Px, Py) :-
    logic:player_win(P),
    draw_move_hive(Window, Px, Py),
    logic:turn(Turno),
    draw_turn(Turno, Window),
    findall(_,
            draw_are_players(Window, Size, Px, Py, 1),
            _),
    findall(_,
            draw_are_players(Window, Size, Px, Py, 2),
            _),
    logic:visit_board(V),
    logic:update_Generic1(V, [], visit_board),
    findall(_, draw_hive(Window, Size), _),
    win(P, Window).


draw_select_bug(ID, Window, Size, Xm, Ym) :-
    logic:to_move(ID),
    new_hexag(Window, [Xm, Ym], Size, blue).
draw_select_bug(_, _, _, _, _). 



play_IA(Window) :-
    game_over(0),
    ia:is_play_IA(1),
    ia:select_best_jugada(red),
    logic:dimention_board(Px, Py),
    clean_board(Window),
    draw_board(Window, 40, Px, Py), !.

play_IA(_) :- !.


play_IA2(Window, Color) :-
    game_over(0),
    logic:turn(C),
    C==Color,
    ia:select_best_jugada(Color),
    logic:dimention_board(Px, Py),
    clean_board(Window),
    draw_board(Window, 40, Px, Py), !.
play_IA2(_, _) :- !.


draw_turn(red, Window) :-
    logic:dimention_board(Px, _),
    draw_image(Window, _, turno2, Px-400, 30), !.


draw_turn(black, Window) :-
    logic:dimention_board(Px, Py),
    draw_image(Window, _, turno1, Px-400, Py-60), !.

draw_turn(_, _) :- !.

draw_are_players(Window, Size, Px, _, 1) :-
    
    %player 1
    draw_line(Window, [0, 80], [Px, 80], black),
    logic:types_bug(Types),
    member(T, Types),
    Y is 40,
    logic:pos_picture_player(T, X),
    new_hexag(Window, [X, Y], Size, black),
    new_hexag(Window, [X, Y], Size-4, red),
    Ix is X-22,
    Iy is Y-22,
    draw_image(Window, _, T, Ix, Iy),
    logic:cant_fichasxtype(T, Number, red),
    draw_number_fichas(Number, [X+43, Y+10], Window),
    logic:selected_type(T),
    logic:turn(red),
    T\=nan,
    logic:pos_picture_player(T, P),
    new_hexag(Window, [P, 40], Size, blue).    
    

draw_are_players(Window, Size, Px, Py, 2) :-
    
    %player 2
    draw_line(Window, [0, Py-80], [Px, Py-80], black),
    logic:types_bug(Types),
    member(T, Types),
    Y is Py-40,
    logic:pos_picture_player(T, X),
    new_hexag(Window, [X, Y], Size, black),
    new_hexag(Window, [X, Y], Size-4, black),
    Ix is X-22,
    Iy is Y-22,
    draw_image(Window, _, T, Ix, Iy),
    logic:cant_fichasxtype(T, Number, black),
    draw_number_fichas(Number, [X+43, Y+10], Window),
    logic:selected_type(T),
    logic:turn(black),
    T\=nan,
    logic:pos_picture_player(T, P),
    new_hexag(Window, [P, Py-40], Size, blue).   


draw_number_fichas(1, [X, Y], Window) :-
    draw_image(Window, _, one, X, Y).


draw_number_fichas(2, [X, Y], Window) :-
    draw_image(Window, _, two, X, Y).


draw_number_fichas(3, [X, Y], Window) :-
    draw_image(Window, _, thre, X, Y).


draw_number_fichas(0, [X, Y], Window) :-
    draw_image(Window, _, cero, X, Y).

complement_ia(1, 0) :- !.
complement_ia(0, 1) :- !.

change_IA() :-
    game_over(0),
    ia:is_play_IA(A),
    complement_ia(A, NA),
    ia:update(A, NA, is_play_IA), !.
    

draw_move_hive(Window, Px, Py) :-
    new(@buttonup, button("up", message(@prolog, move_pos, Window, 1))),
    new(@buttondown, button("dawn", message(@prolog, move_pos, Window, 2))),
    new(@buttonleft, button("left", message(@prolog, move_pos, Window, 3))),
    new(@buttonright, button("right", message(@prolog, move_pos, Window, 4))),
    new(@buttonIA, button("change IA", message(@prolog, change_IA))),
    new(@buttonPlay1,
        button("juega IA", message(@prolog, play_IA2, Window, black))),
    new(@buttonPlay2,
        button("juega IA", message(@prolog, play_IA2, Window, red))),
    send(Window, display, @buttonPlay2, point(Px-500, 40)),
    send(Window, display, @buttonPlay1, point(Px-500, Py-60)),
    send(Window, display, @buttonIA, point(Px-100, 40)),
    send(Window, display, @buttonup, point(Px-100, Py-60)),
    send(Window, display, @buttondown, point(Px-100, Py-40)),
    send(Window, display, @buttonleft, point(Px-150, Py-50)),
    send(Window, display, @buttonright, point(Px-50, Py-50)).


move_pos(Window, Pos) :-
    move_pos_temp(Pos),
    clean_board(Window),
    draw_board(Window, 40, 1366, 700).


clean_board(Window) :-
    send(Window, clear),
    send(@buttonPlay1, free),
    send(@buttonPlay2, free),
    send(@buttonup, free),
    send(@buttonIA, free),
    send(@buttondown, free),
    send(@buttonleft, free),
    send(@buttonright, free).

%up
move_pos_temp(1) :-
    midel_pixel([X, Y]),
    logic:update_Generic1([X, Y], [X, Y-5], midel_pixel).

%dawn
move_pos_temp(2) :-
    midel_pixel([X, Y]),
    logic:update_Generic1([X, Y], [X, Y+5], midel_pixel).

%left
move_pos_temp(3) :-
    midel_pixel([X, Y]),
    logic:update_Generic1([X, Y], [X-5, Y], midel_pixel).

%right
move_pos_temp(4) :-
    midel_pixel([X, Y]),
    logic:update_Generic1([X, Y], [X+5, Y], midel_pixel).

move_or_push(Window, Position) :-
    game_over(0),
    get(Position, x, ClickX),
    get(Position, y, ClickY),
    %hacer seleccionar el tipo de bicho que quiere jugar
    pixel_to_cord(ClickX, ClickY, 40, [Q1, R1]),
    move_or_push_temp(ClickX, ClickY, Window, Q1, R1).
    


play_IA1(1, Window) :-
    play_IA(Window), !.

play_IA1(_, _) :- !.

move_or_push_temp(X, Y, Window, _, _) :-
    logic:dimention_board(Px, Py),
    Y>Py-80, !,
    logic:turn(black),
    select_type(X),
    logic:to_move(ID),
    logic:update_Generic1(ID, 0, to_move),
    logic:selected_type(T),
    logic:num_ficha(N),
    logic:select_ficha(T, black, N),
    clean_board(Window),
    draw_board(Window, 40, Px, Py),
    play_IA1(N, Window). 


move_or_push_temp(X, Y, Window, _, _) :-
    Y<80, !,
    logic:turn(red),
    select_type(X),
    logic:to_move(ID),
    logic:update_Generic1(ID, 0, to_move),
    logic:selected_type(T),
    logic:num_ficha(N),
    logic:select_ficha(T, red, N),
    logic:dimention_board(Px, Py),
    clean_board(Window),
    draw_board(Window, 40, Px, Py). 



move_or_push_temp(_, Y, Window, Q, R) :-
    Y>80,
    logic:dimention_board(Px, Py),
    logic:selected_type(T),
    T\=nan,
    %cambiar por poner ficha
    logic:turn(C),
    logic:push_ficha(T, C, Q, R),
    
    %logic:add_ficha(9, Q, R, T, black),
    logic:update_Generic1(T, nan, selected_type),
    clean_board(Window), !,
    draw_board(Window, 40, Px, Py),
    play_IA(Window).

move_or_push_temp(_, _, Window, Q, R) :-
    %desmarcar la ficha que se hiba a colocar
    logic:selected_type(T),
    logic:update_Generic1(T, nan, selected_type),
    logic:dimention_board(Px, Py),
    logic:to_move(IDMov),
    tablero(IDMov, _, _, TypeMove, Color),
    TypeMove==beetle,
    
    %poner llamar a calcular las posiciones validas del movimiento
    logic:mov_ficha(TypeMove, Color, Q, R),
    clean_board(Window), !,
    draw_board(Window, 40, Px, Py),
    play_IA(Window).

%aaaaaa
move_or_push_temp(_, _, Window, Q, R) :-
    %desmarcar la ficha que se hiba a colocar
    logic:selected_type(T),
    logic:update_Generic1(T, nan, selected_type),
    logic:dimention_board(Px, Py),
    logic:to_move(IDMov),
    tablero(IDMov, _, _, TypeMove, Color),
    TypeMove==pillbug,
    %poner llamar a calcular las posiciones validas del movimiento
    logic:mov_ficha(TypeMove, Color, Q, R),
    clean_board(Window), !,
    draw_board(Window, 40, Px, Py),
    logic:play_pillbug(P),
    play_IA_after_pillbug(P, Window).


move_or_push_temp(_, _, Window, Q, R) :-
    %desmarcar la ficha que se hiba a colocar
    logic:selected_type(T),
    logic:update_Generic1(T, nan, selected_type),
    logic:dimention_board(Px, Py),
    logic:to_move(IDMov),
    tablero(IDMov, _, _, TypeMove, Color),
    TypeMove==mosquito,
    %poner llamar a calcular las posiciones validas del movimiento
    logic:mov_ficha(TypeMove, Color, Q, R),
    clean_board(Window), !,
    draw_board(Window, 40, Px, Py),
    logic:play_pillbug(P),
    play_IA_after_pillbug(P, Window).




move_or_push_temp(_, _, Window, Q, R) :-
    logic:to_move(IDMov),
    logic:tablero(ID, Q, R, Type, C),
    logic:turn(C),
    logic:update_Generic1(IDMov, ID, to_move),
    logic:mov_valid_ficha(Type, C, Q, R),
    update_to_move_empty(),
    logic:dimention_board(Px, Py),
    clean_board(Window), !,
    draw_board(Window, 40, Px, Py).




move_or_push_temp(_, _, Window, Q, R) :-
    logic:dimention_board(Px, Py),
    logic:to_move(IDMov),
    IDMov\=0,
    logic:tablero(IDMov, _, _, Type, Color),
    %llamar a mover ficha
    %logic:delete_ficha(IDMov),
    %logic:add_ficha(IDMov, Q, R, Type, Color),
    logic:mov_ficha(Type, Color, Q, R),
    clean_board(Window), !,
    draw_board(Window, 40, Px, Py),
    play_IA(Window).


play_IA_after_pillbug(1, _) :- !.


play_IA_after_pillbug(0,Window):-
    play_IA(Window).


update_to_move_empty():-
    logic:valid_positions(V),
    length(V, L),
    L==0,
    to_move(ID),
    logic:update_Generic1(ID,0,to_move).
update_to_move_empty().
    

    


select_type(X) :-
    X>10,
    X<70,
    logic:selected_type(T),
    logic:update_Generic1(T, spider, selected_type).
    

select_type(X) :-
    X>110,
    X<170,
    logic:selected_type(T),
    logic:update_Generic1(T, beetle, selected_type).
    

select_type(X) :-
    X>210,
    X<270,
    logic:selected_type(T),
    logic:update_Generic1(T, grasshopper, selected_type).
   

select_type(X) :-
    X>310,
    X<370,
    logic:selected_type(T),
    logic:update_Generic1(T, ladybug, selected_type).


select_type(X) :-
    X>410,
    X<470,
    logic:selected_type(T),
    logic:update_Generic1(T, mosquito, selected_type).
    

select_type(X) :-
    X>510,
    X<570,
    logic:selected_type(T),
    logic:update_Generic1(T, pillbug, selected_type).
   

select_type(X) :-
    X>610,
    X<670,
    logic:selected_type(T),
    logic:update_Generic1(T, queen_bee, selected_type).
    

select_type(X) :-
    X>710,
    X<770,
    logic:selected_type(T),
    logic:update_Generic1(T, soldier_ant, selected_type).
    



start_game :-
    logic:dimention_board(Px, Py),
    new(Window, window('Game', size(Px, Py))),
    send(Window,
         recogniser,
         click_gesture(left,
                       '',
                       single,
                       message(@prolog, move_or_push, Window, @event?position))),
    Size is 40,
    draw_board(Window, Size, Px, Py),
    send(Window, open).

%:-start_game.