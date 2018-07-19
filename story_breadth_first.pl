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

% take an action and add its description to the history
logged_action(StateIn, StateOut) :-
    action(StateIn, S0, Description),
    log(S0, Description, StateOut).
    
print_history([]).
print_history([Format-Values|Tail]) :-
    format(Format, Values),
    print_history(Tail).

% limit story length to avoid infinite recursion
action(State, _, _) :-
    member(history(History), State),
    length(History, N),
    N > 15,
    !,
    fail.

% pee
action(StateIn, StateOut, 'Pee~n'-[]) :-
    member(player_in(bathroom(_)), StateIn),
    select(bladder(full), StateIn, bladder(empty), State0),
    select(hands(_), State0, hands(dirty), StateOut).

% dress for work
action(StateIn, StateOut, 'Dress for work~n'-[]) :-
    member(player_in(closet), StateIn),
    select(dressed_for(bed), StateIn, dressed_for(work), StateOut).

% wash hands
action(StateIn, StateOut, 'Wash hands~n'-[]) :-
    member(player_in(kitchen), StateIn),
    select(hands(dirty), StateIn, hands(clean), StateOut).

% eat
action(StateIn, StateOut, 'Eat~n'-[]) :-
    member(player_in(kitchen), StateIn),
    member(hands(clean), StateIn),
    select(stomach(empty), StateIn, stomach(full), StateOut).

% grab object
action(StateIn, StateOut, 'Grab ~w~n'-[Object]) :-
    member(player_in(Location), StateIn),
    \+ member(holding(_), StateIn),
    select(object_in(Object, Location), StateIn, holding(Object), StateOut).

% move from room to room
action(StateIn, StateOut, 'Move from ~w to ~w~n'-[CurrentLocation, Location]) :-
    member(player_in(CurrentLocation), StateIn),
    connected_to(CurrentLocation, Location),
    select(player_in(CurrentLocation), StateIn, player_in(Location), StateOut).

% drop object
action(StateIn, StateOut, 'Drop ~w~n'-[Object]) :-
    member(player_in(Location), StateIn),
    select(holding(Object), StateIn, object_in(Object, Location), StateOut).

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
    findall(S1, logged_action(HeadState,S1), States),
    append(TailStates, States, Queue),
    process_queue(Queue, StateOut).
