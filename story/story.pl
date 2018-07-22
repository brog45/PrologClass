:- use_module(story_breadth_first).
:- use_module(story_data).

go :-
    init(State),
    time(call_with_time_limit(30, calculate_plan(State, Plan))),
    writeln(Plan).

% vim: et ts=4 sw=4 ai
