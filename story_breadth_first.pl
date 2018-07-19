% breadth-first version, avoids infinite recursion

init(State) :-
    State = [ player_in(bedroom)
            , stomach(empty)
            , bladder(full)
            , hands(dirty)
            , dressed_for(bed)
            , object_in(keys, kitchen)
            , goal(player_in(car))
            , goal(stomach(full))
            , goal(bladder(empty))
            , goal(holding(keys))
            , goal(dressed_for(work))
            , history([])
            ].

done(State) :-
    findall(G, member(goal(G), State), Goals),
    intersection(Goals, State, Goals).

door(yard, car).
door(den, yard).
door(den, kitchen).
door(hall, den).
door(hall, bathroom(guest)).
door(bedroom, hall).
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

% pee
action(StateIn, StateOut, 'Pee~n'-[]) :-
    member(player_in(bathroom(_)), StateIn),
    select(bladder(full), StateIn, bladder(empty), State0),
    select(hands(_), State0, hands(dirty), StateOut).

% wash hands in the bathroom
action(StateIn, StateOut, 'Wash hands~n'-[]) :-
    member(player_in(bathroom(_)), StateIn),
    select(hands(dirty), StateIn, hands(clean), StateOut).

% dress for work
action(StateIn, StateOut, 'Dress for work~n'-[]) :-
    member(player_in(closet), StateIn),
    \+ member(holding(_), StateIn),
    select(dressed_for(bed), StateIn, dressed_for(work), StateOut).

% wash hands in the kitchen
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
    process_queue([State], [], StateOut),
    !,
    member(history(H), StateOut),
    print_history(H).

% process_queue(L, _) :-
%     length(L, N),
%     format('Queue depth ~d~n', N),
%     fail.
process_queue([HeadState|_], _, StateOut) :-
    done(HeadState),
    !,
    log(HeadState, 'Done!~n'-[], StateOut).
process_queue([HeadState|TailStates], ClosedList, StateOut) :-
    findall(S, take_action(HeadState, ClosedList, S), States),
    append(TailStates, States, Queue),
    close_state(ClosedList, HeadState, ClosedList0),
    process_queue(Queue, ClosedList0, StateOut).

% take an action; check its outcome against the closed list; and add its description to the history
take_action(StateIn, ClosedList, StateOut) :-
        action(StateIn, S0, Description),
        delete(S0, history(_), S1),
        \+ member(S1, ClosedList),
        log(S0, Description, StateOut).
    
close_state(ClosedListIn, State, ClosedListOut) :-
    delete(State, history(_), State0),
    append(ClosedListIn, [State0], ClosedListOut).
