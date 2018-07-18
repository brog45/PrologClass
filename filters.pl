:- module(filters, 
          [ less_than_three/1
          ]).

less_than_three(N) :-
    integer(N),
    N < 3.
