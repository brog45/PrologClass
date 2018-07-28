:- module(executor,[generate_story/2]).
:- use_module(story_data).
:- use_module(planner).

generate_story(StateIn, StoryOut) :-
    calculate_plan(StateIn, Plan),
    apply_plan(StateIn, Plan, _, StoryOut).

apply_plan(State, [], State, []).
apply_plan(StateIn, [Action|_], StateOut, [Event|TailOut]) :-
    choose_event(Action, Event, EventDict),
    !,
    apply_action(StateIn, EventDict, NewState),
    calculate_plan(NewState, NewPlan),
    apply_plan(NewState, NewPlan, StateOut, TailOut).
apply_plan(StateIn, [Action|T], StateOut, [Action|TailOut]) :-
    action(Action, ActionDict), 
    apply_action(StateIn, ActionDict, NewState),
    apply_plan(NewState, T, StateOut, TailOut).

choose_event(Action, Event, ActionDict) :-
    random(Luck), !,
    event(Action, Probability, Event, ActionDict),
    Probability >= Luck,
    debug(executor(choose_event), 'Replacing ~w with ~w', [Action, Event]).