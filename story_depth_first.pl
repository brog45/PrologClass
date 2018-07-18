% depth-first and prone to infinite recursion if door data is not arranged just so

init(State) :-
    State = [ player_in(bedroom)
            , history([])
            ].

done(State) :-
    member(player_in(kitchen), State).

% door from hall to den is listed above door to bathroom to avoid depth-first infinite recursion
door(den, kitchen).
door(hall, den).
door(hall, bathroom).
door(bedroom, hall).

connected_to(A,B) :- door(A,B).
connected_to(A,B) :- door(B,A).

log(State, FormatPair, State0) :-
    member(history(History), State),
    append(History, [FormatPair], History0),
    select(history(History), State, history(History0), State0).

print_history([]).
print_history([Format-Values|Tail]) :-
    format(Format, Values),
    print_history(Tail).

action(StateIn, StateOut) :-
    member(player_in(CurrentLocation), StateIn),
    connected_to(CurrentLocation, Location),
    select(player_in(CurrentLocation), StateIn, player_in(Location), State0),
    log(State0, 'Move from ~w to ~w~n'-[CurrentLocation, Location], StateOut).

go :-
    init(State),
    !,
    next(State, State0),
    !,
    member(history(H), State0),
    print_history(H).

next(StateIn, StateOut) :- 
    (   done(StateIn)
    ->  log(StateIn, 'Done!~n'-[], StateOut)
    ;   action(StateIn, State0),
        next(State0, StateOut)
    ).
