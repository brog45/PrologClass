% story data

:- module(story_data,[init/1, action/1]).

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

% pee
action(action{
        prereqs: [player_in(bathroom(_)), bladder(full)],
        negprereqs: [holding(_)],
        removes: [hands(_), bladder(full)],
        adds: [hands(dirty), bladder(empty)],
        description: [pee]
    }).

% wash hands in the bathroom
action(action{
        prereqs: [player_in(bathroom(_)), hands(dirty)],
        negprereqs: [holding(_)],
        removes: [hands(dirty)],
        adds: [hands(clean)],
        description: [wash_hands]
    }).

% dress for work
action(action{
        prereqs: [player_in(closet)],
        negprereqs: [holding(_)],
        removes: [dressed_for(bed)],
        adds: [dressed_for(work)],
        description: [dress(work)]
    }).

% wash hands in the kitchen
action(action{
        prereqs: [player_in(kitchen), hands(dirty)],
        negprereqs: [holding(_)],
        removes: [hands(dirty)],
        adds: [hands(clean)],
        description: [wash_hands]
    }).

% eat
action(action{
        prereqs: [player_in(kitchen), hands(clean), stomach(empty)],
        negprereqs: [holding(_)],
        removes: [stomach(empty)],
        adds: [stomach(full)],
        description: [eat]
    }).

% grab object
action(action{
        prereqs: [player_in(Location), object_in(Object, Location)],
        negprereqs: [holding(_)],
        removes: [object_in(Object, Location)],
        adds: [holding(Object)],
        description: [grab(Object)]
    }).

% move from room to room
action(action{
        prereqs: [player_in(CurrentLocation)],
        negprereqs: [],
        removes: [player_in(CurrentLocation)],
        adds: [player_in(Location)],
        description: [move(CurrentLocation, Location)]
    }) :-
    connected_to(CurrentLocation, Location).

% drop object
action(action{
        prereqs: [player_in(Location), holding(Object)],
        negprereqs: [],
        removes: [holding(Object)],
        adds: [object_in(Object, Location)],
        description: [drop(Object)]
    }).
