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
action(action{
        prereqs: [player_in(bathroom(_)), bladder(full)],
        negprereqs: [holding(_)],
        removes: [hands(_), bladder(full)],
        adds: [hands(dirty), bladder(empty)],
        description: 'Pee~n'-[]
    }).

% wash hands in the bathroom
action(action{
        prereqs: [player_in(bathroom(_)), hands(dirty)],
        negprereqs: [holding(_)],
        removes: [hands(dirty)],
        adds: [hands(clean)],
        description: 'Wash hands~n'-[]
    }).

% dress for work
action(action{
        prereqs: [player_in(closet)],
        negprereqs: [holding(_)],
        removes: [dressed_for(bed)],
        adds: [dressed_for(work)],
        description: 'Dress for work~n'-[]
    }).

% wash hands in the kitchen
action(action{
        prereqs: [player_in(kitchen), hands(dirty)],
        negprereqs: [holding(_)],
        removes: [hands(dirty)],
        adds: [hands(clean)],
        description: 'Wash hands~n'-[]
    }).

% eat
action(action{
        prereqs: [player_in(kitchen), hands(clean), stomach(empty)],
        negprereqs: [holding(_)],
        removes: [stomach(empty)],
        adds: [stomach(full)],
        description: 'Eat~n'-[]
    }).

% grab object
action(action{
        prereqs: [player_in(Location), object_in(Object, Location)],
        negprereqs: [holding(_)],
        removes: [object_in(Object, Location)],
        adds: [holding(Object)],
        description: 'Grab ~w~n'-[Object]
    }).

% move from room to room
action(action{
        prereqs: [player_in(CurrentLocation)],
        negprereqs: [],
        removes: [player_in(CurrentLocation)],
        adds: [player_in(Location)],
        description: 'Move from ~w to ~w~n'-[CurrentLocation, Location]
    }) :-
    connected_to(CurrentLocation, Location).

% drop object
action(action{
        prereqs: [player_in(Location), holding(Object)],
        negprereqs: [],
        removes: [holding(Object)],
        adds: [object_in(Object, Location)],
        description: 'Drop ~w~n'-[Object]
    }).



action(StateIn, StateOut, Description) :-
    action(Dict), 
    apply_action(StateIn, Dict, StateOut),
    Description = Dict.description.

apply_action(StateIn, Dict, StateOut) :-
    subtract(StateIn, Dict.negprereqs, StateIn),
    intersection(Dict.prereqs, StateIn, Dict.prereqs),
    subtract(StateIn, Dict.removes, S0),
    append(S0, Dict.adds, StateOut).

go :-
    init(State),
    !,
    ord_empty(ClosedSet),
    process_queue([State], ClosedSet, StateOut),
    !,
    memberchk(history(H), StateOut),
    print_history(H).

process_queue(OpenList, ClosedSet, _) :-
    length(OpenList, OpenLen),
    length(ClosedSet, ClosedLen),
    debug(planner(process_queue), 'open ~w closed ~w', [OpenLen, ClosedLen]),
    fail.
process_queue([HeadState|_], _, StateOut) :-
    done(HeadState),
    !,
    log(HeadState, 'Done!~n'-[], StateOut).
process_queue([HeadState|TailStates], ClosedSet, StateOut) :-
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
