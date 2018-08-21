:- use_module(library(clpfd)).

puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-
    Vars = [S,E,N,D,M,O,R,Y],
    Vars ins 0..9,
    all_different(Vars),
              S*1000 + E*100 + N*10 + D +
              M*1000 + O*100 + R*10 + E #=
    M*10000 + O*1000 + N*100 + E*10 + Y,
    M #\= 0, S #\= 0,
    label(Vars).

puzzle2([F,O,R,T,Y] + [T,E,N] + [T,E,N] = [S,I,X,T,Y]) :-
    Vars = [F,O,R,T,Y,E,N,S,I,X],
    Vars ins 0..9,
    all_different(Vars),
    F*10_000 + O*1000 + R*100 + T*10 + Y +
                        T*100 + E*10 + N +
                        T*100 + E*10 + N #=
    S*10_000 + I*1000 + X*100 + T*10 + Y,
    F #\= 0, T #\= 0, T #\= 0, 
    label(Vars).

increase([H|T]) :- increase_(H, T).
increase_(_, []).
increase_(X, [H|T]) :- 
    X #< H,
    increase_(H, T).

% Bases is a list of ints. Constrain Var to be within B..B+2 for B
% a member of Bases

two_domains(Bases, Var) :-
    make_two_domains(Bases, Term),
    Var in Term.

make_two_domains([H], H..HT) :-
    HT is H + 2.
make_two_domains([H | T], H..HT \/ TDomain) :-
    HT is H + 2,
    make_two_domains(T, TDomain).
