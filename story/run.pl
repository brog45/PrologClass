:- use_module(planner).
:- use_module(story_data).
:- use_module(story_dcg).
:- use_module(story_generator).

go :-
    init(State),
    generate_story(State, Story),
    phrase(story(Story), StoryCodes),
    !,
    string_codes(StoryString, StoryCodes),
    writeln(StoryString).

% vim: et ts=4 sw=4 ai
