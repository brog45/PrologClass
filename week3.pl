% https://swish.swi-prolog.org/p/qXEEGEnH.swinb

%!      list_sum(+List:list_of_numbers, -Sum:int) is det
%
%    Succeeds if Sum is the sum of the numbers in List
%    
%    @arg List a list of numbers
%    @arg Sum their sum
%
list_sum([], X) :- !, X = 0.
list_sum([Head|Tail], Sum) :-
    is_list(Head),
    !,
    list_sum(Head, 0, Accumulator),
    list_sum(Tail, Accumulator, Sum).
list_sum([Head|Tail], Sum) :-
    % \+ is_list(Head),
    list_sum(Tail, Head, Sum).
% list_sum/3 below uses an accumulator 
list_sum([], Accumulator, Accumulator).
list_sum([Head|Tail], Accumulator, Sum) :-
    is_list(Head),
    !,
    list_sum(Head, Accumulator, NextAccumulator),
    list_sum(Tail, NextAccumulator, Sum).
list_sum([Head|Tail], Accumulator, Sum) :-
    number(Head),
    !,
    NextAccumulator is Accumulator + Head,
    list_sum(Tail, NextAccumulator, Sum).

:- begin_tests(list_sum).

test(list_sum) :- list_sum([], 0).
test(list_sum, [fail]) :- list_sum([], 1).
test(list_sum) :- list_sum([1], 1).
test(list_sum) :- list_sum([1,2], 3).
test(list_sum) :- list_sum([1,2,4], 7).
test(list_sum) :- list_sum([1,[2,4]], 7).
test(list_sum, [ forall(append(X,Y,[1,2,4])) ]) :- list_sum([X,Y], 7).
test(list_sum, [fail]) :- list_sum([1,b, c(7)], _).
test(list_sum, [fail]) :- list_sum(_,4).

:- end_tests(list_sum).

%!   nth_in_list(+List:list, +Index:integer, -Value:term) is semidet
%
%   extract the 0 based Index'th element of List
%
%   This predicate requires Index is an integer greater than or equal 
%   to 0 and less than the length of List; otherwise, it fails. 
%   
%   @arg List    list to extract element from
%   @arg Index   index - 0 is first element
%   @arg Value   will be bound to the Index'th element on return
%
nth_in_list([Head|_], 0, Head) :- !.
nth_in_list([_|Tail], N, Value) :-
    N1 is N - 1,
    nth_in_list(Tail, N1, Value).

% # Category Exercise

person(X) :- member(X, [
    alice,
    bob,
    charlene,
    derek,
    ethyl,
    faye,
    gilam,
    harvey
    ]).
household(X) :- member(X, [
       phone,
       sheet,
       bed,
       chair,
       cup]).
industry(X) :- member(X, [
      factory,
      laboratory,
      machine_shop,
      foundry
      ]).

mixed_stuff([ethyl, tacos, charlene, factory,
    phone, sheet, harvey]).

%!   stuff_category(?Stuff:list_of_stuff, ?Cat:list_of_categories) is nondet
%
%    @arg Stuff a list of atoms
%    @arg Cat a list of what category the element is in
%
stuff_category(Items, Categories) :- 
    maplist(item_category, Items, Categories).
% This item_category clause prevents known values of Item from backtracking and 
% hitting Category = none.
item_category(Item, Category) :- 
    ground(Item), !,
    (   person(Item), !, Category = person
    ;   household(Item), !, Category = household
    ;   industry(Item), !, Category = industry
    ;   Category = none
    ).
% These remaining clauses of item_category work when Item is unbound and allow
% backtracking to generate all possible solutions.
item_category(Item, person) :- person(Item).
item_category(Item, household) :- household(Item).
item_category(Item, industry) :- industry(Item).
item_category(_, none).

:- begin_tests(stuff_category).

test(stuff_category1) :- stuff_category([], []).
test(stuff_category2) :- 
    stuff_category([ethyl, cup, yesterday, factory], 
                   [person, household, none, industry]).
test(stuff_category3, [forall(stuff_category(Stuff, [person, person, industry]))]) :- 
    Stuff = [A, B, C],
    person(A),
    person(B),
    industry(C),
    !.
test(stuff_category4, [forall(stuff_category([S1, phone, S2], [person, household, Cat1]))]) :- 
    person(S1), 
    !,
    item_category(S2, Cat1).
test(stuff_category5) :- stuff_category([alice], [person]).
test(stuff_category6, [fail]) :- stuff_category([alice], [none]).
test(stuff_category7, [fail]) :- stuff_category([alice], [household]).
test(stuff_category8, [fail]) :- stuff_category([alice], [industry]).
test(stuff_category9) :- stuff_category([cup], [household]).
test(stuff_category10, [fail]) :- stuff_category([cup], [none]).
test(stuff_category11, [fail]) :- stuff_category([cup], [person]).
test(stuff_category12, [fail]) :- stuff_category([cup], [industry]).
test(stuff_category13) :- stuff_category([factory], [industry]).
test(stuff_category14, [fail]) :- stuff_category([factory], [none]).
test(stuff_category15, [fail]) :- stuff_category([factory], [person]).
test(stuff_category16, [fail]) :- stuff_category([factory], [household]).
test(stuff_category17, [forall(person(X))]) :- stuff_category([X], [person]).
test(stuff_category18, [forall(household(X))]) :- stuff_category([X], [household]).
test(stuff_category19, [forall(industry(X))]) :- stuff_category([X], [industry]).

:- end_tests(stuff_category).

%!     filter(:Goal:callable, +List:list, -Filtered:list) is det
%
%    Succeeds if the last argument is a list of those elements
%    of the second argument for which the first argument
%    succeeds at least once
%    
%    @arg Goal  the goal to test with an added arg
%    @arg List  the input list
%    @arg Filtered those elements of List for which Goal succeeds
%
filter(_, [], []) :- !.
filter(Goal, [X|Xs], Filtered) :-
    filteracc(Goal, [X|Xs], Filtered, []).
% filter/3 wraps filteracc/4, which uses a difference 
% list and a hole to avoid reversing the list order.
filteracc(_, [], Hole, Hole) :- !.
filteracc(Goal, [X|Xs], [X|Filtered], Hole) :-
    call(Goal, X), 
    !,
    filteracc(Goal, Xs, Filtered, Hole).
filteracc(Goal, [_|Xs], Filtered, Hole) :-
    filteracc(Goal, Xs, Filtered, Hole).

% always/1 is used in the tests below
always(_).

:- begin_tests(filter).

test(filter1) :- filter(=(a), [], X), X = [].
test(filter2) :- filter(=(a), [a,b,c], X), X = [a].
test(filter3) :- filter(=(d), [a,b,c], X), X = [].
test(filter4) :- filter(always, [a,b,c], X), X = [a,b,c].

:- end_tests(filter).
