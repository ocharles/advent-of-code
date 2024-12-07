:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(pio)).

input(Rules, Updates) --> rules(Rules), ['\n'], updates(Updates).

% Rs is a list of X|Y rules.
rules([X-Y|Rs]) --> integer(X), "|", integer(Y), ['\n'], rules(Rs).
rules([])       --> [].

% Us is a list of updates.
updates([U|Us]) --> update(U), ['\n'], updates(Us).
updates([])     --> [].

% Us is a single list of page updates.
update([U|Us]) --> integer(U), ",", update(Us).
update([U])    --> integer(U).

% N in an integer, parsed as a sequence of ASCII digits.
integer(N) --> digits(Ds), { number_chars(N, Ds) }.

% A run of one or more ASCII digits.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D])    --> digit(D).

% D is a single ASCII digit.
digit(D) --> [D], { char_code('0', Zero), char_code(D, Code), Code >= Zero, Code =< Zero + 9 }.

not_eq(X, Y) :- X #\= Y.

% True iff all updates in Us respect the given update dependencies.
valid_update(Rules, Us) :- valid_update(Rules, Us, []).

valid_update(_, [], _).
valid_update(Rules, [U|Us], Bad) :-
    findall(X, member(X-U, Rules), Xs),
    maplist(not_eq(U), Bad),
    append(Bad, Xs, Bads),
    valid_update(Rules, Us, Bads).

% X is the middle element of a list.
mid(X) --> seq(As), [X], seq(Bs), { length(As, N), length(Bs, N) }.

part_1(File, N) :-
	once(phrase_from_file(input(Rules, Updates), File)),
	findall(U, (member(U, Updates), valid_update(Rules, U, [])), Valids), 
	findall(M, (member(V, Valids), once(phrase(mid(M), V))), Mids), 
	sum(Mids, #=, N).

% Correct an invalid update by re-ordering it so it satisfies all rules.
correct(Rules, Corrected, Corrected) :- valid_update(Rules, Corrected).
correct(Rules, [X|Xs], Corrected) :-
	correct(Rules, Xs, Ys),
	select(X, Corrected, Ys),
	valid_update(Rules, Corrected).

% N in the sum of the middle update of all invalid updates.
part_2(File, N) :-
	once(phrase_from_file(input(Rules, Updates), File)),
	findall(U, (member(U, Updates), \+ valid_update(Rules, U, [])), Invalids), 
	maplist(correct(Rules), Invalids, Valids),
	findall(M, (member(V, Valids), once(phrase(mid(M), V))), Mids), 
	sum(Mids, #=, N).
