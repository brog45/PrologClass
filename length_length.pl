meters_per(ft, 0.3048).
meters_per(inches, 0.0254).
meters_per(cm, 0.01).

length_length(U, V) :- 
    ground(U), !, length_length_(V, U).
length_length(U, V) :- 
    ground(V), !, length_length_(U, V).
length_length_(U, V) :-
    ground(V),
    U =.. [Unit1, Q1],
    V =.. [Unit2, Q2],
    meters_per(Unit1, F1),
    meters_per(Unit2, F2),
    Q1 is F2*Q2/F1.

% Rounding errors make it necessary to test that values 
% are within a tolerable range of each other.
near(X, Y) :- D is abs(X-Y), D < 0.01.

:- begin_tests(length_length).

test(x, [true(near(Inches, 36))]) :- length_length(ft(3), inches(Inches)).
test(x, [true(near(Inches, 96))]) :- length_length(ft(8), inches(Inches)).
test(x, [true(near(Feet, 3))]) :- length_length(ft(Feet), inches(36)).
test(x, [true(near(CM, 2.54))]) :- length_length(inches(1), cm(CM)).
test(x, [fail]) :- length_length(ft(_), inches(_)).

:- end_tests(length_length).
