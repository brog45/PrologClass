:- multifile prolog:message//1.

print_banner :-
    print_message(debug(foo), annies_amazing_thing(7,3,23)).

prolog:message(annies_amazing_thing(Major,Minor,Revision)) -->
    [ 'Annie\'s Amazing Thing', nl
    , 'Does Something Or Other', nl
    , 'Rev ~d.~d.~d'-[Major,Minor,Revision], nl
    ].

:- multifile user:message_property/2.

%user:message_property(debug(foo), color([fg(yellow), bg(blue)])).
user:message_property(debug(foo), prefix('BANNER: ')).
