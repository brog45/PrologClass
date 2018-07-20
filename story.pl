:- use_module(story_breadth_first).
:- use_module(story_data).
:- init(State, 'Brian', 'Murray', cat),
	time(call_with_time_limit(30, go(State))).
:- halt.
