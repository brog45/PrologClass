% broken.pl
:- module(broken, []).
:- use_module(add).
:- use_module(filters).

go :-
    add_some([1,2,3,4,5], less_than_three, S),
    writeln(S).
