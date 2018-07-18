as --> [].
as --> [a], as.

ab --> [].
ab --> [a], ba.
ba --> [].
ba --> [b], ab.

% cliche
%
cliche -->
    {
        cliche_triple(Thing, ThingType, Opposite)
    },
    thing(Thing),
    " is a ", 
    type_of_thing(ThingType), 
    " trapped in a ", 
    opposite_type_of_thing(Opposite), 
    " body.".
thing(Thing) --> Thing.
type_of_thing(ThingType) --> ThingType.
opposite_type_of_thing(Opposite) --> Opposite.

cliche_triple("Cygwin", "Unix OS", "Windows'").
cliche_triple("Fluffy", "dog", "cat's").
cliche_triple("Bob the swimmer", "fish", "human's").

% fizzbuzz -- sort of
%
fizz_buzz(Msg) --> anything, fizz(Msg), anything, buzz(Msg), anything.
anything --> [].
anything --> [_], anything.
fizz(Msg) -->
    "fizz",
    {
        format('At fizz we have Msg=~w~n', [Msg])
    }.
buzz(Msg) -->
    "buzz",
    {
        format('At buzz we have Msg=~w~n', [Msg])
    }.

%
%
sentence --> noun_phrase, " ", verb_phrase.
noun_phrase --> determiner, " ", noun.
verb_phrase --> verb, " ", noun_phrase.
determiner --> ("a"; "an"; "the").
noun --> "book".
noun --> "car".
noun --> "man".
verb --> "greets".
verb --> "operates".
verb --> "reads".

%
%
something(X) --> 
    (   { is_wobbly(X) }  
    ->  "a wobbly ",
        thing 
    ;   "a stable ",
        thing
     ).
thing --> "table".
thing --> "chair".
is_wobbly(wobbly).
