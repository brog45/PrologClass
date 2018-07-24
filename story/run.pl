:- use_module(planner).
:- use_module(story_data).
:- use_module(story_dcg).
:- use_module(executor).

go :-
    init(State),
    time(call_with_time_limit(30, calculate_plan(State, Plan))),
    apply_plan(State, Plan, _, PlanOut),
    phrase(story(PlanOut), StoryCodes),
    !,
    string_codes(StoryString, StoryCodes),
    writeln(StoryString).

% vim: et ts=4 sw=4 ai
