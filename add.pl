:- module(add, 
          [ add_all/2
          , add_some/3 
          ]).

:- meta_predicate 
        add_some(+, 1, -).

%! add_all(+List:integer_list, -Sum:integer)
%
% add_all/2 should succeed iff the first parameter 
% is a list of integers whose sum is equal to the second.
add_all(L, Sum) :- add_all_(L, 0, Sum).

add_all_([], Accumulator, Sum) :-
    !, Sum = Accumulator.
add_all_([X|Xs], Accumulator, Sum) :-
    integer(X),
    NewAccumulator is Accumulator + X,
    add_all_(Xs, NewAccumulator, Sum).

:- begin_tests(add_all).

test('[], X -> X = 0', true(X = 0)) :- add_all([], X).
test('[], 0 passes') :- add_all([], 0).
test('[], 1 fails', [fail]) :- add_all([], 1).
test('[1,2,3], X -> X = 6', true(X = 6)) :- add_all([1,2,3], X).
test('[1.0] fails', [fail]) :- add_all([1.0], _).
test('[a] fails', [fail]) :- add_all([a], _).
test('a fails', [fail]) :- add_all(a, _).
test('0 fails', [fail]) :- add_all(0, _).
test('_, 1 fails', [fail]) :- add_all(_, 1).
test('[_], 0 fails', [fail]) :- add_all([_], 0).

:- end_tests(add_all).

%! add_some(?, ?, ?)
%
% add_some/3 should succeed iff the first parameter is a list 
% of integers, the second a Goal applied to each member of the 
% first and the final is a number equal to the sum of all 
% members of the first parameter for which the goal in the 
% second succeeded.
add_some(L, Goal, Sum) :- add_some_(L, Goal, 0, Sum).

add_some_([], _, Accumulator, Sum) :-
    !, Sum = Accumulator.
add_some_([X|Xs], Goal, Accumulator, Sum) :-
    call(Goal, X), 
    !, NewAccumulator is Accumulator + X,
    add_some_(Xs, Goal, NewAccumulator, Sum).
add_some_([_|Xs], Goal, Accumulator, Sum) :-
    add_some_(Xs, Goal, Accumulator, Sum).
