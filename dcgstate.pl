num_leaves(nil), [N1] --> [N0], { N1 is N0 + 1 }.
num_leaves(node(_,Left,Right)) -->
    num_leaves(Left),
    num_leaves(Right).

/** <examples>

?- phrase(num_leaves(node(a,node(b,nil,nil),nil)), [0], [N]).

*/
