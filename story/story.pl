:- use_module(planner).
:- use_module(story_data).
:- use_module(story_dcg).

go :-
    init(State),
    time(call_with_time_limit(30, calculate_plan(State, Plan))),
    writeln(Plan),
    phrase(story(Plan), StoryCodes),
    string_codes(StoryString, StoryCodes),
    writeln(StoryString).

% vim: et ts=4 sw=4 ai
