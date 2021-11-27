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
:- consult(main),
   import(logic).



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
    X is Size*(sqrt(3)*Q+R*sqrt(3)/2)+Px,
    Y is Size*(R*3/2)+Py.


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
    print("ok"),
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


draw_hive(X, Y, Q, R, ID, Type, Color, Window, Size) :-
    logic:visit_board(V),
    not(logic:isContain(ID, V)),
    logic:concat(V, [ID], NV),
    logic:update_Generic1(V, NV, visit_board),
    new_hexag(Window, [X, Y], Size, black),
    new_hexag(Window, [X, Y], Size-4, Color),
    draw_select_bug(ID, Window, Size-6, X, Y),
    Ix is X-22,
    Iy is Y-22,
    draw_image(Window, _, Type, Ix, Iy),
    numlist(1, 6, Ady),
    member(Pos, Ady),
    logic:get_posicion_ady(Pos, Dq, Dr),
    Nq is Q+Dq,
    Nr is R+Dr,
    logic:tablero(N, Nq, Nr, T, C),
    print(N),
    get_midel_pixel(X,
                    Y,
                    Size,
                    Pos,
                    Nx,
                    Ny),
    draw_hive(Nx,
              Ny,
              Nq,
              Nr,
              N,
              T,
              C,
              Window,
              Size).


    
draw_board(Window, Size, Px, Py) :-
    draw_move_hive(Window, Px, Py),
    findall(_,
            draw_are_players(Window, Size, Px, Py, 1),
            _),
    findall(_,
            draw_are_players(Window, Size, Px, Py, 2),
            _),
    logic:visit_board(V),
    logic:update_Generic1(V, [], visit_board),
    logic:midel_pixel([Pxm, Pym]),
    findall(_,
            draw_hive(Pxm,
                      Pym,
                      0,
                      0,
                      1,
                      spider,
                      red,
                      Window,
                      Size),
            _).


draw_select_bug(ID, Window, Size, Xm, Ym) :-
    logic:to_move(ID),
    new_hexag(Window, [Xm, Ym], Size, blue).
draw_select_bug(_, _, _, _,_). 

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
    draw_image(Window, _, T, Ix, Iy).    
    

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
    logic:selected_type(T),
    T\=nan,
    logic:pos_picture_player(T, P),
    new_hexag(Window, [P, Py-40], Size, blue).   

draw_move_hive(Window, Px, Py) :-
    new(@buttonup, button("up", message(@prolog, move_pos, Window, 1))),
    new(@buttondown, button("dawn", message(@prolog, move_pos, Window, 2))),
    new(@buttonleft, button("left", message(@prolog, move_pos, Window, 3))),
    new(@buttonright, button("right", message(@prolog, move_pos, Window, 4))),
    %point(40,Py-40),
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
    send(@buttonup, free),
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
    get(Position, x, ClickX),
    get(Position, y, ClickY),
    %hacer seleccionar el tipo de bicho que quiere jugar
    pixel_to_cord(ClickX, ClickY, 40, [Q1, R1]),
    move_or_push_temp(ClickX, ClickY, Window, Q1, R1).
    %vambiar por poner ficha
    %logic:add_ficha(9, Q1, R1, spider, red),
    %new_hexag(Window, [ClickX, ClickY], 40, black),
    %clean_board(Window),
    %draw_board(Window, 40, 1366, 700).
move_or_push_temp(X, Y, Window, _, _) :-
    logic:dimention_board(Px, Py),
    Y>Py-80,
    select_type(X, Px, Py, Window). 

move_or_push_temp(_, Y, Window, Q, R) :-
    Y>80,
    logic:dimention_board(Px, Py),
    logic:selected_type(T),
    T\=nan,
    %cambiar por poner ficha
    logic:add_ficha(9, Q, R, T, black),
    logic:update_Generic1(T, nan, selected_type),
    clean_board(Window),
    draw_board(Window, 40, Px, Py).

move_or_push_temp(_, _, Window, Q, R) :-
    logic:dimention_board(Px, Py),
    logic:tablero(ID, Q, R, _, _),
    logic:to_move(IDMov),
    logic:update_Generic1(IDMov, ID, to_move),
    clean_board(Window),
    draw_board(Window, 40, Px, Py).

move_or_push_temp(_, _, Window, Q, R):-
    logic:dimention_board(Px, Py),
    logic:to_move(IDMov),
    IDMov\=0,
    logic:tablero(IDMov,_,_,Type,Color),
    logic:delete_ficha(IDMov),
    logic:add_ficha(IDMov,Q,R,Type,Color),
    clean_board(Window),
    draw_board(Window, 40, Px, Py).
    


select_type(X, Px, Py, Window) :-
    X>10,
    X<70,
    logic:selected_type(T),
    logic:update_Generic1(T, spider, selected_type),
    clean_board(Window),
    draw_board(Window, 40, Px, Py).

select_type(X, Px, Py, Window) :-
    X>110,
    X<170,
    logic:selected_type(T),
    logic:update_Generic1(T, beetle, selected_type),
    clean_board(Window),
    draw_board(Window, 40, Px, Py).

select_type(X, Px, Py, Window) :-
    X>210,
    X<270,
    logic:selected_type(T),
    logic:update_Generic1(T, grasshopper, selected_type),
    clean_board(Window),
    draw_board(Window, 40, Px, Py).

select_type(X, Px, Py, Window) :-
    X>310,
    X<370,
    logic:selected_type(T),
    logic:update_Generic1(T, ladybug, selected_type),

    clean_board(Window),
    draw_board(Window, 40, Px, Py).


select_type(X, Px, Py, Window) :-
    X>410,
    X<470,
    logic:selected_type(T),
    logic:update_Generic1(T, mosquito, selected_type),
    clean_board(Window),
    draw_board(Window, 40, Px, Py).

select_type(X, Px, Py, Window) :-
    X>510,
    X<570,
    logic:selected_type(T),
    logic:update_Generic1(T, pillbug, selected_type),
    clean_board(Window),
    draw_board(Window, 40, Px, Py).

select_type(X, Px, Py, Window) :-
    X>610,
    X<670,
    logic:selected_type(T),
    logic:update_Generic1(T, queen_bee, selected_type),
    clean_board(Window),
    draw_board(Window, 40, Px, Py).

select_type(X, Px, Py, Window) :-
    X>710,
    X<770,
    logic:selected_type(T),
    logic:update_Generic1(T, soldier_ant, selected_type),
    clean_board(Window),
    draw_board(Window, 40, Px, Py).



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
    %draw_move_hive(Window, Px, Py),
    draw_board(Window, Size, Px, Py),
    send(Window, open).

%:-start_game.