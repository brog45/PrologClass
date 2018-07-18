:- module(pldoc_demo, []).
/** <module> Demo of pldoc functionality
 */

:- multifile  quiz_predicate/1.

%!   quiz_predicate(+X:list) is det
%
%   predicate to demo pldoc functionality
quiz_predicate(X) :-
    writeln(X).
