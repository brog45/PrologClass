zseq(zero(N)) --> [0], zseq_(zero(N0)), {succ(N0, N), !}.
zseq_(zero(N)) --> [0], zseq_(zero(N0)), {succ(N0, N), !}.
zseq_(zero(0)) --> [].

:- begin_tests(zseq).

    test(empty, [fail]) :- phrase(zseq(_), []).
    test(one, [all(Z == [zero(1)])]) :- phrase(zseq(Z), [0]).
    test(two, [all(Z == [zero(2)])]) :- phrase(zseq(Z), [0,0]).
    test(three, [all(Z == [zero(3)])]) :- phrase(zseq(Z), [0,0,0]).

:- end_tests(zseq).

sparse([]) --> \+ [_], !.
sparse([zero(N)|T]) --> zseq(zero(N)), !, sparse(T).
sparse([H|T]) --> [H], {H \= 0}, sparse(T).

:- begin_tests(sparse).

    test(sparse, 
         [ forall(member(Sparse-Packed, 
                         [ []-[]
                         , [1]-[1]
                         , [0]-[zero(1)]
                         , [0,0]-[zero(2)]
                         , [0,0,1]-[zero(2),1]
                         , [1,0,0]-[1,zero(2)]
                         ]))
         , true(OutPacked == Packed)
         ]) :-
        phrase(sparse(OutPacked), Sparse).

    test(reverse, 
         [ blocked(broken_but_optional)
         , forall(member(Sparse-Packed, 
                         [ [0]-[zero(1)]
                        %, []-[]
                        %, [1]-[1]
                        %, [0,0]-[zero(2)]
                        %, [0,0,1]-[zero(2),1]
                        %, [1,0,0]-[1,zero(2)]
                         ]))
         , true(OutSparse == Sparse)
         ]) :-
        phrase(sparse(Packed), OutSparse).

:- end_tests(sparse).
