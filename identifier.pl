identifier([H|T]) --> identifier_start([H]), identifier_rest(T).
identifier_start([C]) --> (underscore([C]) ; alpha([C])), !.
identifier_rest([H|T]) --> (underscore([H]) ; alnum([H])), !, identifier_rest(T).
identifier_rest([]) --> [], !.
underscore(`_`) --> "_".
alpha([A]) --> [A], {code_type(A, alpha)}.
alnum([A]) --> [A], {code_type(A, alnum)}.

:- begin_tests(identifier).

test(good, 
     [ forall(member(String, ["_", "_x", "x", "X", "_1x", "abc_123_", "goodId", "GoodId"]))
     , true(string_codes(String, IdentCodes))
     ]) :-
    string_codes(String, Codes),
    phrase(identifier(IdentCodes), Codes).

test(bad, 
     [ forall(member(String, ["", " ", "1", "x 1", "123_abc_"]))
     , fail
     ]) :-
    string_codes(String, Codes),
    phrase(identifier(_), Codes).

:- end_tests(identifier).
