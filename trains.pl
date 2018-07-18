/*
Train Routes Exercise

Programming Exercise.

Download the accompanying file, which gives pairs of cities that have a 
passenger train running between them. Assume if a train runs one way, it 
runs the other. So you can get from seattle to vancouver.

Write a SWI-Prolog program that provides a predicate, route_length/3, that 
succeeds when the first two arguments are two cities and the last argument is 
the number of stops between them, including the final stop (so 
route_length(vancouver, portland, Len) should bind Len to 2.

You'll encounter a couple problems. First, if you go vancouver-seattle, you're 
going to find seattle-vancouver and end up in a loop.  So you'll need to hang 
on to not only where you are, but where you just came from. You'll need the \= 
operator (does not unify). 

Second, there's a loop in the data. You can give up if you've gone more hops 
than there are stations in the data.
*/

train(vancouver, seattle).
train(seattle, portland).
train(spokane, minneapolis).
train(minneapolis, milwaukee).
train(chicago, milwaukee).
train(dubuque, chicago).
train(chicago, omaha).
train(omaha, denver).
train(salt_lake_city, denver).
train(salt_lake_city, sacramento).
train(sacramento, oakland).
train(sacramento, portland).
train(portland, spokane).
train(oakland, san_jose).
train(san_jose, los_angeles).
train(los_angeles, albuquerque).
train(albuquerque, kansas_city).
train(kansas_city, chicago).

% The tracks between cities are bidirectional
connects_to(A,B) :-
    train(A,B).
connects_to(A,B) :-
    train(B,A).

%!  route_length(+A:city, +B:city, -Length:int) is nondet
%!  route_length(-A:city, +B:city, +Length:int) is nondet
%   Length is the number of hops from A to B.
route_length(A,A,0).
route_length(A,B,1) :-
    connects_to(A,B).
route_length(A,B,N) :-
    route_length_l([A],B,N).
route_length_l([A|As],B,N) :-
    connects_to(A,B),
    length([A|As], N).
route_length_l([A|As],B,N) :-
    connects_to(A,C),     % there is some adjacent city
    C \= B,               % that is not our destination
    \+ member(C, [A|As]), % that we have not already visited
    route_length_l([C,A|As],B,N).
