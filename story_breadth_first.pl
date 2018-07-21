% breadth-first version, avoids infinite recursion

:- module(story_breadth_first,[go/1]).

done(State) :-
    findall(G, member(goal(G), State), Goals),
    intersection(Goals, State, Goals).

% add a message to the history
log(State, Format-Values, State0) :-
    member(history(History), State),
    append(History, [Format-Values], History0),
    select(history(History), State, history(History0), State0).

print_history([]).
print_history([Format-Values|Tail]) :-
    format(Format, Values),
    print_history(Tail).

go(InitialState) :-
    ord_empty(ClosedSet),
    process_queue([InitialState], ClosedSet, StateOut),
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
    % action/1 is not defined inside this module and should be defined in the story data
    action(Action, ActionDict),
    action_is_applicable(ActionDict, StateIn),
    outcome(Action, OutcomeDict),
    apply_outcome(StateIn, OutcomeDict, S0),
    state_not_closed(S0, ClosedSet),
    log(S0, OutcomeDict.description, StateOut).

action_is_applicable(ActionDict, State) :-
    subtract(State, ActionDict.negprereqs, State),
    intersection(ActionDict.prereqs, State, ActionDict.prereqs).

apply_outcome(StateIn, OutcomeDict, StateOut) :-
    subtract(StateIn, OutcomeDict.removes, S0),
    append(S0, OutcomeDict.adds, StateOut).

state_not_closed(State, ClosedSet) :-
    delete(State, history(_), S1),
    list_to_ord_set(S1, S2),
    \+ ord_memberchk(S2, ClosedSet).

close_state(ClosedSetIn, State, ClosedSetOut) :-
    delete(State, history(_), State0),
    list_to_ord_set(State0, OrdSet),
    ord_add_element(ClosedSetIn, OrdSet, ClosedSetOut).
