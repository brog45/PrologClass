% breadth-first version, avoids infinite recursion

init(State) :-
    State = [ player_in(bedroom)
            , stomach(empty)
            , bladder(full)
            , hands(dirty)
            , dressed_for(bed)
            , object_in(keys, kitchen)
            , goal(player_in(kitchen))
            , goal(stomach(full))
            , goal(bladder(empty))
            , goal(holding(keys))
            , goal(dressed_for(work))
            , history([])
            ].

done(State) :-
    findall(G, member(goal(G), State), Goals),
    intersection(Goals, State, Goals).

door(den, kitchen).
door(bedroom, den).
door(bedroom, bathroom(master)).
door(bathroom(master), closet).

connected_to(A,B) :- door(A,B).
connected_to(A,B) :- door(B,A).

% add a message to the history
log(State, Format-Values, State0) :-
    member(history(History), State),
    append(History, [Format-Values], History0),
    select(history(History), State, history(History0), State0).

print_history([]).
print_history([Format-Values|Tail]) :-
    format(Format, Values),
    print_history(Tail).

% limit story length to avoid infinite recursion
action(State, _) :-
    member(history(History), State),
    length(History, N),
    N > 15,
    !,
    fail.

% pee
action(StateIn, StateOut) :-
    member(player_in(bathroom(_)), StateIn),
    select(bladder(full), StateIn, bladder(empty), State0),
    select(hands(_), State0, hands(dirty), State1),
    log(State1, 'Pee~n'-[], StateOut).

% dress for work
action(StateIn, StateOut) :-
    member(player_in(closet), StateIn),
    select(dressed_for(bed), StateIn, dressed_for(work), State0),
    log(State0, 'Dress for work~n'-[], StateOut).
    
% wash hands
action(StateIn, StateOut) :-
    member(player_in(kitchen), StateIn),
    select(hands(dirty), StateIn, hands(clean), State0),
    log(State0, 'Wash hands~n'-[], StateOut).

% eat
action(StateIn, StateOut) :-
    member(player_in(kitchen), StateIn),
    member(hands(clean), StateIn),
    select(stomach(empty), StateIn, stomach(full), State0),
    log(State0, 'Eat~n'-[], StateOut).

% grab object
action(StateIn, StateOut) :-
    member(player_in(Location), StateIn),
    \+ member(holding(_), StateIn),
    select(object_in(Object, Location), StateIn, holding(Object), State0),
    log(State0, 'Grab ~w~n'-[Object], StateOut).

% move from room to room
action(StateIn, StateOut) :-
    member(player_in(CurrentLocation), StateIn),
    connected_to(CurrentLocation, Location),
    select(player_in(CurrentLocation), StateIn, player_in(Location), State0),
    log(State0, 'Move from ~w to ~w~n'-[CurrentLocation, Location], StateOut).

% drop object
action(StateIn, StateOut) :-
    member(player_in(Location), StateIn),
    select(holding(Object), StateIn, object_in(Object, Location), State0),
    log(State0, 'Drop ~w~n'-[Object], StateOut).

go :-
    init(State),
    !,
    process_queue([State], StateOut),
    !,
    member(history(H), StateOut),
    print_history(H).

% process_queue(L, _) :-
%     length(L, N),
%     format('Queue depth ~d~n', N),
%     fail.
process_queue([HeadState|_], StateOut) :-
    done(HeadState),
    !,
    log(HeadState, 'Done!~n'-[], StateOut).
process_queue([HeadState|TailStates], StateOut) :-
    findall(S1, action(HeadState,S1), States),
    append(TailStates, States, Queue),
    process_queue(Queue, StateOut).
