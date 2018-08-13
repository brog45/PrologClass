:- discontiguous emp_name/2, position/2.

% 1. Implement the conversion from employee/2 to emp_name/2 and position/2

term_expansion(employee(Id, Name, Position), [emp_name(Id, Name), position(Id, Position)]).

employee(1, john, ceo).
employee(2, brian, developer).

% 2. Implement the speedup of sum_up.  Make a loop that runs sum_up on a 
%    fairly short list a large number of times and use profile/1 to see 
%    how much time it takes with and without the speedup.

do_times(0,_) :- !.
do_times(N,Goal) :-
    Goal,
    succ(N1, N),
    do_times(N1,Goal).
        
sum_up(List, Total) :-
    sum_up(0, List, Total).
sum_up(Acc, [], Acc) :- !.
sum_up(Acc, [H|T], Total) :-
    NewAcc is Acc + H,
    sum_up(NewAcc, T, Total).

goal_expansion(sum_up_x(List, Total), sum_up(0, List, Total)).

:- noprofile(do_times/2).

goal_unexpanded :- sum_up([1,2,3,4],_Total).
profile_unexpanded :- reset_profiler, profile(do_times(1_000_000, goal_unexpanded)).
time_unexpanded :- time(do_times(1_000_000, goal_unexpanded)).

goal_expanded :- sum_up_x([1,2,3,4],_Total).
profile_expanded :- reset_profiler, profile(do_times(1_000_000, goal_expanded)).
time_expanded :- time(do_times(1_000_000, goal_expanded)).

% 3. In our big system we used to call crufty_code/2. We've realized we wished
%    it was crufty_code/3, but it's not easy to automate what that extra 
%    argument is. Build a tool that warns about calls to crufty_code/2. Use 
%    print_message to print the warning.

crufty_code(A,B) :-
    format('A=~w~nB=~w~n', [A,B]).

:- multifile prolog:message//1.

prolog:message(deprecated(Term)) -->
    [ 'Use of deprecated predicate ~w'-[Term] ].

goal_expansion(crufty_code(_,_), _) :-
    print_message(warning, deprecated(crufty_code/2)),
    fail.

% A warning about the line below is printed when this file is consulted.
blah :- crufty_code(a,b).

% After manually fixing a bunch of these, you realize the extra argument has 
% the same name all the way through a module. Change your setup so if it runs 
% into a directive :-extra_crufty_arg('X') it adds the variable X. Notice 
% you'll have to use goal_expansion/4 instead of goal_expansion/2 to get the 
% variable name.
