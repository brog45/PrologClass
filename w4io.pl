:- module(w4io,[call_throws/0,term_in_stream/2,term_in_file/2]).


term_in_file(Term, File) :-
    setup_call_catcher_cleanup(open(File, read, In),
                               term_in_stream(Term, In),
                               Catcher,
                               term_in_file_cleanup(Catcher, In) ).

term_in_file_cleanup(Catcher, In) :-
    writeln(Catcher), close(In).

term_in_stream(Term, In) :-
    repeat,
    read(In, T),
    %writeln(T),    
    (   T == end_of_file
    ->  !, fail
    ;   T = Term
    ).

call_throws :-
    term_in_file(b, 'like.txt').
