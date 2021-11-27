%implementacion del for
for(N, N, _) :-!.
for(I, N, Ejecuta) :-
    call(Ejecuta),
    IN is I+1,
    for(IN, N, Ejecuta),!.
for(I, N, Ejecuta) :-
    IN is I+1,
    for(IN, N, Ejecuta).
    


%Concatena dos listas
concat([], X, X).
concat([X|R], Y, [X|Z]) :-
    concat(R, Y, Z).



