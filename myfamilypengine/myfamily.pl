:- module(myfamily,[
            descendant_of/2,
            ancestor_of/2,
            second_cousins_once_removed/2,
            second_cousins/2,
            great_grandparent_of/2,
            first_cousins_once_removed/2,
            first_cousins/2,
            grandparent_of/2,
            sister_of/2,
            brother_of/2,
            uncle_of/2,
            aunt_of/2,
            siblings/2,
            daughter_of/2,
            son_of/2,
            child_of/2,
            father_of/2,
            mother_of/2,
            parent_of/2,
            parents/3,
            female/1,
            male/1]).

/*
# Family Tree Exercise
*/

% These three predicates are discontiguous to allow their God clauses
% and Annie's family clauses to be grouped together.
:- discontiguous male/1, female/1, parent_of/2.

/*
1.  Make a family tree knowledgebase. Use your own family tree, or a
    royal family or fictional character (Lord Of The Rings has some:
    Bilbo's, Kings of Gondor, etc).
*/

%!  male(Person)
%   Person is male.
male(coleman).
male(bennie).
male(john_n).
male(john_r).
male(john_s).
male(kevin).
male(brian).
male(garrett).

%!  female(Person)
%   Person is female.
female(katherine).
female(rosa_mae).
female(teresa).
female(mary).
female(liane).
female(alisha).
female(shari).
female(heather).
female(caitlin).
female(zoe).
female(pat).

%!  parents(Child, Mother, Father)
%   Mother and Father are the parents of Child.
parents(john_r, katherine, coleman).
parents(pat, rosa_mae, bennie).
parents(ben, pat, john_n).
parents(angela, pat, john_n).
parents(debbie, pat, john_n).
parents(terry, pat, john_n).
parents(katie, angela, gene).
parents(spencer, debbie, frank).
parents(brittany, debbie, frank).
parents(cara, debbie, frank).
parents(jaimee, terry, don).
parents(erin, terry, don).
parents(teresa, rosa_mae, bennie).
parents(shari, teresa, john_r).
parents(kevin, teresa, john_r).
parents(brian, teresa, john_r).
parents(alisha, teresa, john_r).
parents(heather, mary, john_s).
parents(dylan, alisha, steve).
parents(zoe, alisha, steve).
parents(caitlin, heather, brian).
parents(garrett, heather, brian).
parents(rose, jaimee, william).
parents(sophia, jaimee, william).

/*
2.  Add rules for the following relationships: mother, father, son,
    daughter, brother, sister, uncle, cousin, second cousin once removed,
    ancestor, and descendant. Ancestor and descendant should be capable
    of going back or forward arbitrarily far. Add enough people that an
    example of each relationship is in the knowledgebase. Feel free to
    make up people if  you need to.

3.  Provide comments and use proper indenting and style - mother_of is a
    better name than mother for that predicate.
*/

%!  parent_of(Parent, Child)
%   Parent is a parent of Child.
parent_of(Parent, Child) :-
    parents(Child, Parent, _).
parent_of(Parent, Child) :-
    parents(Child, _, Parent).

%!  mother_of(Mother, Child)
%   Mother is the mother of Child.
mother_of(Mother, Child) :-
    female(Mother),
    parent_of(Mother, Child).

%!  father_of(Father, Child)
%   Father is the father of Child.
father_of(Father, Child) :-
    male(Father),
    parent_of(Father, Child).

%!  child_of(Child, Parent)
%   Child is a child of Parent.
child_of(Child, Parent) :-
    parent_of(Parent, Child).

%!  son_of(Son, Parent)
%   Son is a male child of Parent.
son_of(Son, Parent) :-
    male(Son),
    child_of(Son, Parent).

%!  daughter_of(Daughter, Parent)
%   Daughter is a female child of Parent.
daughter_of(Daughter, Parent) :-
    female(Daughter),
    child_of(Daughter, Parent).

%!  siblings(A, B)
%   A and B share a parent
siblings(A, B) :-
    parent_of(Parent, A),
    parent_of(Parent, B),
    Parent \= god, % otherwise everybody is siblings
    A \= B.

%!  aunt_of(Aunt, Child)
%   Aunt is an aunt of Child.
aunt_of(Aunt, Child) :-
    parent_of(Parent, Child),
    sister_of(Aunt, Parent).

%!  uncle_of(Uncle, Child)
%   Uncle is an uncle of Child.
uncle_of(Uncle, Child) :-
    parent_of(Parent, Child),
    brother_of(Uncle, Parent).

%!  brother_of(Brother, Sibling)
%   Brother is a male sibling of Sibling.
brother_of(Brother, Sibling) :-
    male(Brother),
    siblings(Brother, Sibling).

%!  sister_of(Sister, Sibling)
%   Sister is a female sibling of Sibling.
sister_of(Sister, Sibling) :-
    female(Sister),
    siblings(Sister, Sibling).

%!  grandparent_of(Grandparent, Grandchild)
%   Grandparent is a grandparent of Grandchild.
grandparent_of(Grandparent, Grandchild) :-
    parent_of(Grandparent, Parent),
    parent_of(Parent, Grandchild).

%!  first_Cousins(Cousin1, Cousin2)
%   Cousin1 and Cousin2 are first cousins, i.e.: they share a grandparent.
first_cousins(Cousin1, Cousin2) :-
    grandparent_of(Grandparent, Cousin1),
    grandparent_of(Grandparent, Cousin2),
    Grandparent \= god, % otherwise everybody is first cousins
    \+ siblings(Cousin1, Cousin2),
    Cousin1 \= Cousin2.

%!  first_cousins_once_removed(Cousin1, Cousin2)
%   Cousin1 and Cousin2 are first cousins once removed, i.e.: one is the child
%   of a first cousin of the other.
first_cousins_once_removed(Cousin1, Cousin2) :-
    parent_of(Parent, Cousin1),
    first_cousins(Parent, Cousin2).
first_cousins_once_removed(Cousin1, Cousin2) :-
    parent_of(Parent, Cousin2),
    first_cousins(Parent, Cousin1).

great_grandparent_of(GreatGrandparent, GreatGrandchild) :-
    parent_of(GreatGrandparent, Grandparent),
    grandparent_of(Grandparent, GreatGrandchild).

%!  second_cousins(Cousin1, Cousin2)
%   Cousin1 and Cousin2 are second cousins, i.e.: they share a
%   great-grandparent.
second_cousins(Cousin1, Cousin2) :-
    great_grandparent_of(GreatGrandparent, Cousin1),
    great_grandparent_of(GreatGrandparent, Cousin2),
    GreatGrandparent \= god, % otherwise everybody is second cousins
    \+ first_cousins(Cousin1, Cousin2),
    \+ siblings(Cousin1, Cousin2),
    Cousin1 \= Cousin2.

%!  second_cousins_once_removed(Cousin1, Cousin2)
%   Cousin1 and Cousin2 are second cousins once removed, i.e.: one is the child
%   of a second cousin of the other.
second_cousins_once_removed(Cousin1, Cousin2) :-
    parent_of(Parent, Cousin1),
    second_cousins(Parent, Cousin2).
second_cousins_once_removed(Cousin1, Cousin2) :-
    parent_of(Parent, Cousin2),
    second_cousins(Parent, Cousin1).

ancestor_of(Ancestor, Descendant) :-
    parent_of(Ancestor, Descendant).
ancestor_of(Ancestor, Descendant) :-
    parent_of(Parent, Descendant),
    ancestor_of(Ancestor, Parent).

descendant_of(Descendant, Ancestor) :-
    ancestor_of(Ancestor, Descendant).

/*
4.  The MCC Church believes that God is everyone's father and mother, and
    hence both male and female. Add this belief (and God) to your program.
*/

% God is male
male(god).
% God is female
female(god).
% God is everybody's mother and father too. God is not His own, though;
% otherwise, ancestor_of/2 recurses infintely.
parent_of(god,Whom) :-
    Whom \= god.

/*
5.  How would you make an atheist version of your program? How would you
    characterize the 'beliefs' of your program before you did step 4?

An agnostic, rather than presupposing the existence of God in the knowledgebase,
might write a rule god(X) that succeeds where X is male and female and every
person (but X) in the knowledgebase is a child of X. An atheist may choose to
represent as fact that there is no God. One way they could do that is as a rule
that always fails, e.g.:

    god(_) :- !, fail.

Before adding a belief in God to the system, the system was much simpler.
*/

/*
6.  The next item in Moodle is my own family tree data. Look at it only after
    you write your own version. Add my family into your scheme, without altering
    the data provided. Just copy and paste, (you can rename things to avoid name
    collisions). Make rules that bridge 'your way' and 'my way'.
*/

% Annie's Family Tree

annies_female(annie_ogborn).
annies_female(rosie_ogborn).
annies_female(esther_boger).
annies_female(mildred_ogborn).
annies_male(don_ogborn).
annies_male(randy_ogborn).
annies_male(mike_ogborn).
annies_male(dicky_boger).
annies_male(george_ogborn).
annies_male(elmer_ogborn).
annies_male(karl_boger).

annies_parent_of(rosie_ogborn, annie_ogborn).
annies_parent_of(rosie_ogborn, randy_ogborn).
annies_parent_of(rosie_ogborn, mike_ogborn).
annies_parent_of(don_ogborn, annie_ogborn).
annies_parent_of(don_ogborn, randy_ogborn).
annies_parent_of(don_ogborn, mike_ogborn).
annies_parent_of(george_ogborn, rosie_ogborn).
annies_parent_of(esther_boger, rosie_ogborn).
annies_parent_of(esther_boger, dicky_boger).
annies_parent_of(karl_boger, dicky_boger).

% clauses for comatibility with Annie's family predicates

male(Person) :-
    annies_male(Person).
female(Person) :-
    annies_female(Person).
parent_of(Child,Parent) :-
    annies_parent_of(Child,Parent).

/*
7.  Add a comment answering these questions:
    - Pick two brothers. Use your program to demonstrate that
      they *are* brothers. How many answers does your query give?
      Explain.
    - In step 4 you added God. What problems did that cause?
      How did you fix them?
    - Step 4 might not accord to some student's religious beliefs.
      What does that say about the nature of truth in a Prolog
      knowledgebase?

Because I defined a brother as a male sibling and a sibling as a person who
shares a parent, there are two solutions to the following query, i.e: Kevin 
and Brian are brothers by virtue of sharing a mother and also by virtue of 
sharing a father.

    ?- brother_of(brian, kevin).
    true ;
    true ;
    false.

When I first added God to the system, everybody was siblings, so nobody was first 
cousins. I had to add a "\= god" exception, but then everybody was first_cousins,
so I added an exception to first_cousins. Then everybody was second cousins, so I 
added an exception to that rule. Any rule that relies on there being some shared 
ancestor would have to also require that the shared ancestor not be God. I also 
had to modify the parent_of God clause so God would not be His own parent, in 
order to prevent infinite recursion in ancestor_of.

Inserting God as a fact into a strictly and narrowly defined system that models
persons breaks the system because the Judeo-Christian God is not a person.
Rather, various metaphors have historically been used in certain contexts to
explain god. Those explanations may be useful in those contexts, but are not
applicable in the context of a family tree. It's not the same context; it's not
the same model. Some of the words used may be the same, but they don't mean quite
the same thing.
*/

/*
8.  Submit your finished program using the second item below this one in Moodle.
*/

% vim: syntax=prolog et sw=4 ts=4 ai