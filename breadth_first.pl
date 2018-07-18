bf_find_path_to(node(Name,Left,Right), Goal, Path) :-
    bf_find_([state(node(Name,Left,Right), [])], Goal, Path).

bf_find_([state(node(Name,_,_), Path)|_], Goal, PathOut) :-
    call(Goal, Name), 
    !,
    PathOut = Path.
bf_find_([state(node(_,Left,Right), Path)|T], Name, PathOut) :-
    append(Path, [left], PathLeft),
    append(Path, [right], PathRight),
    StateLeft = state(Left, PathLeft),
    StateRight = state(Right, PathRight),
    append(T, [StateLeft, StateRight], T0),
    !, 
    bf_find_(T0, Name, PathOut).
bf_find_([state(nil, _)|T], Name, PathOut) :-
    !, 
    bf_find_(T, Name, PathOut).

tree(node(root, 
          node(a, 
               node(b, 
                    node(c, 
                         node(d,nil,nil), 
                         nil), 
                    nil),
               node(x, 
                    nil, 
                    node(y, 
                         node(z,nil,nil),
                         nil))), 
          node(d,nil,nil))
    ).

in_list(Xs,X) :- member(X,Xs).
