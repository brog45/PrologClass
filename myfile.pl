sum_list(List, Sum) :-
    sum_list(List, 0, Sum).

sum_list([],  Sum, Sum).    % bind the output arg here in base case
sum_list([H|T], SoFar, Sum) :-
    NewSum is H + SoFar,
    sum_list(T, NewSum, Sum).

:- begin_tests(blah).

test(or, all(X == [1, 2])) :-
    (X = 1 ; X = 2).

:- end_tests(blah).
