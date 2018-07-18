% tree_nodes(nil) --> [].
% tree_nodes(node(Name, Left, Right)) --> 
%     tree_nodes(Left), 
%     [Name], 
%     tree_nodes(Right).

tree_nodes(nil, Ls, Ls) --> [].
tree_nodes(node(Name, Left, Right), [_|Ls0], Ls) -->
    tree_nodes(Left, Ls0, Ls1),
    [Name],
    tree_nodes(Right, Ls1, Ls).

