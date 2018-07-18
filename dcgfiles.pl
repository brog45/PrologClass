like(What) --> "I like ", list(What), ".", list(_).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).

/*
phrase(like(What), "I like it. The rest is ignored").

use_module(library(pio)).
phrase_from_file(like(What), 'like.txt').
*/