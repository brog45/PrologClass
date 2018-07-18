/* to avoid introducing modules, I'm not putting this in a module.
 * Not a good production practice to have code not in modules.
 *
 * Instead, I've tried to make my names unlikely to collide with
 * yours.
 *
 * Oh, and comments are like in C
 * Or start with %
 * slash** or %% or %! starts the autodocs thing
 */
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



