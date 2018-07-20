% story data

:- module(story_data,[init/4, action/2, outcome/2]).

%! init(-State:story_state)
init(State, PlayerName, PetName, PetType) :-
    State = [ player_in(bedroom)
            , player(PlayerName)
            , pet(PetName, PetType)
            , stomach(empty)
            , bladder(full)
            , hands(dirty)
            , dressed_for(bed)
            , object_in(keys, bedroom)
            % , object_in(comb, bathroom)
            % , goal(player_in(car))
            , goal(player_in(kitchen))
            , goal(stomach(full))
            , goal(bladder(empty))
            , goal(holding(keys))
            , goal(dressed_for(work))
            , goal(hands(clean))
            , goal(pet_happy)
            , history(['You wake up in your bedroom. Time to get ready for work!~n'-[]])
            ].

door(bedroom, kitchen).
door(bedroom, bathroom).
door(bathroom, closet).

connected_to(A,B) :- door(A,B).
connected_to(A,B) :- door(B,A).

:- discontiguous action/2, outcome/2.

% call in sick
action(sick_day, action{
        prereqs: [goal(sick_day)],
        negprereqs: [sick_day]
    }).
outcome(sick_day, outcome{
        removes: [],
        adds: [sick_day],
        description: 'You call your boss and take a sick day.~n'-[]
    }).

% love pet
action(love_pet(Name, Animal), action{
        prereqs: [pet_demands_love, pet(Name, Animal)],
        negprereqs: []
    }).
outcome(love_pet(Name, Animal), outcome{
        removes: [pet_demands_love, hands(clean)],
        adds: [pet_happy, hands(dirty)],
        description: 'You pet ~w. He is now a happy ~w.~n'-[Name, Animal]
    }).

% pee
action(pee, action{
        prereqs: [player_in(bathroom), bladder(full)],
        negprereqs: [holding(_)]
    }).
outcome(pee, outcome{
        removes: [hands(_), bladder(full)],
        adds: [hands(dirty), bladder(empty)],
        description: 'You use the restroom.~n'-[]
    }).

% wash hands
action(wash_hands, action{
        prereqs: [player_in(bathroom), hands(dirty)],
        negprereqs: [holding(_)]
    }).
action(wash_hands, action{
        prereqs: [player_in(kitchen), hands(dirty)],
        negprereqs: [holding(_)]
    }).
outcome(wash_hands, outcome{
        removes: [hands(dirty)],
        adds: [hands(clean)],
        description: 'You wash your hands.~n'-[]
    }).

% dress for work
action(dress, action{
        prereqs: [player_in(closet), dressed_for(bed)],
        negprereqs: [holding(_), pet_demands_love]
    }).
outcome(dress, outcome{
        removes: [dressed_for(bed)],
        adds: [dressed_for(work)],
        description: 'You dress for work.~n'-[]
    }).

% eat
action(eat, action{
        prereqs: [player_in(kitchen), hands(clean), stomach(empty)],
        negprereqs: [holding(_), pet_demands_love]
    }).
outcome(eat, outcome{
        removes: [stomach(empty)],
        adds: [stomach(full)],
        description: 'You eat.~n'-[]
    }).
outcome(eat, outcome{
        removes: [stomach(empty), goal(_)],
        adds: [stomach(full), goal(sick_day)],
        description: 'You eat but feel sick afterward. Better take a sick day.~n'-[]
    }).

% grab object
action(grab(Object), action{
        prereqs: [player_in(Location), object_in(Object, Location)],
        negprereqs: [holding(_)]
    }).
outcome(grab(Object), outcome{
        removes: [object_in(Object, _)],
        adds: [holding(Object)],
        description: 'You pick up the ~w.~n'-[Object]
    }).

% move from room to room
action(move(CurrentLocation, Location, pet(Name,Type)), action{
        prereqs: [player_in(CurrentLocation), pet(Name, Type)],
        negprereqs: [pet_demands_love]
    }) :-
    connected_to(CurrentLocation, Location).
outcome(move(CurrentLocation, Location, pet(Name,Type)), outcome{
        removes: [player_in(CurrentLocation), pet_happy],
        adds: [player_in(Location), pet_demands_love],
        description: 'As you go from the ~w to the ~w, your ~w ~w demands love.~n'-[CurrentLocation, Location, Type, Name]
    }).
outcome(move(CurrentLocation, Location, _), outcome{
        removes: [player_in(CurrentLocation)],
        adds: [player_in(Location)],
        description: 'You go from the ~w to the ~w.~n'-[CurrentLocation, Location]
    }).

% drop object
action(drop(Object, Location), action{
        prereqs: [player_in(Location), holding(Object)],
        negprereqs: []
    }).
outcome(drop(Object, Location), outcome{
        removes: [holding(Object)],
        adds: [object_in(Object, Location)],
        description: 'You put down the ~w.~n'-[Object]
    }).
