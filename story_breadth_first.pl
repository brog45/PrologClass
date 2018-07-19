% breadth-first version, avoids infinite recursion

init(State) :-
    State = [ player_in(bedroom)
            , stomach(empty)
            , bladder(full)
            , hands(clean)
            , dressed_for(bed)
            , object_in(keys, bedroom)
            , object_in(comb, bathroom(master))
            % , goal(player_in(car))
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

% door(yard, car).
% door(den, yard).
door(den, kitchen).
door(hall, den).
% door(hall, bathroom(guest)).
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
    \+ member(holding(_), StateIn),
    select(hands(_), State0, hands(dirty), StateOut).

% wash hands in the bathroom
action(StateIn, StateOut, 'Wash hands~n'-[]) :-
    member(player_in(bathroom(_)), StateIn),
    \+ member(holding(_), StateIn),
    select(hands(dirty), StateIn, hands(clean), StateOut).

% dress for work
action(StateIn, StateOut, 'Dress for work~n'-[]) :-
    member(player_in(closet), StateIn),
    \+ member(holding(_), StateIn),
    select(dressed_for(bed), StateIn, dressed_for(work), StateOut).

% wash hands in the kitchen
action(StateIn, StateOut, 'Wash hands~n'-[]) :-
    member(player_in(kitchen), StateIn),
    \+ member(holding(_), StateIn),
    select(hands(dirty), StateIn, hands(clean), StateOut).

% eat
action(StateIn, StateOut, 'Eat~n'-[]) :-
    member(player_in(kitchen), StateIn),
    member(hands(clean), StateIn),
    \+ member(holding(_), StateIn),
    select(stomach(empty), StateIn, stomach(full), StateOut).


% % This is faster than the data-driven approach below
% % grab object
% action(StateIn, StateOut, 'Grab ~w~n'-[Object]) :-
%     member(player_in(Location), StateIn),
%     \+ member(holding(_), StateIn),
%     select(object_in(Object, Location), StateIn, holding(Object), StateOut).


% grab object
action(StateIn, StateOut, 'Grab ~w~n'-[Object]) :-
    PreReqs = [player_in(Location), object_in(Object, Location)],
    NegPreReqs = [holding(_)],
    Removes = [object_in(Object, Location)],
    Adds = [holding(Object)],
    subtract(StateIn, NegPreReqs, StateIn),
    intersection(PreReqs, StateIn, PreReqs),
    subtract(StateIn, Removes, S0),
    append(S0, Adds, StateOut).

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
    ord_empty(ClosedSet),
    process_queue([State], ClosedSet, StateOut),
    !,
    memberchk(history(H), StateOut),
    print_history(H).

% process_queue(L, _) :-
%     length(L, N),
%     format('Queue depth ~d~n', N),
%     fail.
process_queue([HeadState|_], _, StateOut) :-
    done(HeadState),
    !,
    log(HeadState, 'Done!~n'-[], StateOut).
process_queue([HeadState|TailStates], ClosedSet, StateOut) :-
    length(TailStates, OpenLen),
    length(ClosedSet, ClosedLen),
    debug(planner(process_queue), 'open ~w closed ~w', [OpenLen, ClosedLen]),
    findall(S, take_action(HeadState, ClosedSet, S), States),
    append(TailStates, States, Queue),
    close_state(ClosedSet, HeadState, ClosedList0),
    process_queue(Queue, ClosedList0, StateOut).

% take an action; check its outcome against the closed list; and add its description to the history
take_action(StateIn, ClosedSet, StateOut) :-
    action(StateIn, S0, Description),
    delete(S0, history(_), S1),
    list_to_ord_set(S1, S2),
    \+ ord_memberchk(S2, ClosedSet),
    log(S0, Description, StateOut).

close_state(ClosedSetIn, State, ClosedSetOut) :-
    delete(State, history(_), State0),
    list_to_ord_set(State0, OrdSet),
    ord_add_element(ClosedSetIn, OrdSet, ClosedSetOut).
