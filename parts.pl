part(rim).
part(spoke).
part(rearframe).
part(handles).
part(gears).
part(bolt).
part(nut).
part(fork).

assembly(bike, [wheel, wheel, frame]).
assembly(wheel, [spoke,rim,hub]).
assembly(frame, [rearframe, frontframe]).
assembly(frontframe, [fork,handles]).
assembly(hub, [gears,axle]).
assembly(axle, [bolt,nut]).

partsof(X, [Y]) :- part(X), !, X = Y.
partsof(X, P) :- 
    assembly(X, Subparts),
    partsoflist(Subparts, P).

partsoflist([],[]).
partsoflist([P|Tail], Total) :-
    partsof(P, Headparts),
    partsoflist(Tail, Tailparts),
    append(Headparts, Tailparts, Total).

% vim: syntax=prolog ts=4 sw=4 et ai
