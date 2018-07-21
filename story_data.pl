% story data

:- module(story_data,[init/1, action/2, outcome/2]).

%! init(-State:story_state)
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

:- discontiguous action/2, outcome/2.

% pee
action(pee, action{
        prereqs: [player_in(bathroom(_)), bladder(full)],
        negprereqs: [holding(_)]
    }).
outcome(pee, outcome{
        removes: [hands(_), bladder(full)],
        adds: [hands(dirty), bladder(empty)],
        description: 'Pee~n'-[]
    }).

% wash hands
action(wash_hands, action{
        prereqs: [player_in(bathroom(_)), hands(dirty)],
        negprereqs: [holding(_)]
    }).
action(wash_hands, action{
        prereqs: [player_in(kitchen), hands(dirty)],
        negprereqs: [holding(_)]
    }).
outcome(wash_hands, outcome{
        removes: [hands(dirty)],
        adds: [hands(clean)],
        description: 'Wash hands~n'-[]
    }).

% dress for work
action(dress, action{
        prereqs: [player_in(closet), dressed_for(bed)],
        negprereqs: [holding(_)]
    }).
outcome(dress, outcome{
        removes: [dressed_for(bed)],
        adds: [dressed_for(work)],
        description: 'Dress for work~n'-[]
    }).

% eat
action(eat, action{
        prereqs: [player_in(kitchen), hands(clean), stomach(empty)],
        negprereqs: [holding(_)]
    }).
outcome(eat, outcome{
        removes: [stomach(empty)],
        adds: [stomach(full)],
        description: 'Eat~n'-[]
    }).

% grab object
action(grab(Object), action{
        prereqs: [player_in(Location), object_in(Object, Location)],
        negprereqs: [holding(_)]
    }).
outcome(grab(Object), outcome{
        removes: [object_in(Object, _)],
        adds: [holding(Object)],
        description: 'Grab ~w~n'-[Object]
    }).

% move from room to room
action(move(CurrentLocation, Location), action{
        prereqs: [player_in(CurrentLocation)],
        negprereqs: []
    }) :-
    connected_to(CurrentLocation, Location).
outcome(move(CurrentLocation, Location), outcome{
        removes: [player_in(CurrentLocation)],
        adds: [player_in(Location)],
        description: 'Move from ~w to ~w~n'-[CurrentLocation, Location]
    }).

% drop object
action(drop(Object, Location), action{
        prereqs: [player_in(Location), holding(Object)],
        negprereqs: []
    }).
outcome(drop(Object, Location), outcome{
        removes: [holding(Object)],
        adds: [object_in(Object, Location)],
        description: 'Drop ~w~n'-[Object]
    }).
