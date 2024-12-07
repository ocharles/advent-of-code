:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(reif)).

% Xs is each parsed line of input.
lines([X|Xs]) --> line(X), lines(Xs).
lines([])     --> [].

% X-Y is the line "X Y".
line(X-Y) --> integer(X), whitespace, integer(Y), ['\n'].

% N in an integer, parsed as a sequence of ASCII digits.
integer(N) --> digits(Ds), { number_chars(N, Ds) }.

% A run of one or more ASCII digits.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D])    --> digit(D).

% D is a single ASCII digit.
digit(D) --> [D], { char_code('0', Zero), char_code(D, Code), Code >= Zero, Code =< Zero + 9 }.

% One or more spaces.
whitespace --> [' '].
whitespace --> [' '], whitespace.

% Pairs is a list of pairs, where each pair is a pairwise combination of Xs and Ys.
unpairs([X-Y|Pairs], [X|Xs], [Y|Ys]) :- unpairs(Pairs, Xs, Ys).
unpairs([], [], []).

% D is the distance between X and Y.
pair_distance(X, Y, D) :- D #= abs(X - Y).

% N is the solution to part 1 on File.
part_1(File, N) :-
	once(phrase_from_file(lines(Ls), File)),
	unpairs(Ls, Xs, Ys),
	sort(Xs, XsAsc),
	sort(Ys, YsAsc),
	maplist(pair_distance, XsAsc, YsAsc, Distances),
	sum(Distances, #=, N).

% N is how similar X is to the list Ys.
similarity(Ys, X, N) :-
	tfilter(=(X), Ys, Xs),
	length(Xs, NXs),
	N #= X * NXs.

% N is the solution to part 2 on File.
part_2(File, N) :-
	once(phrase_from_file(lines(Ls), File)),
	unpairs(Ls, Xs, Ys),
	maplist(similarity(Ys), Xs, Similarities),
	sum(Similarities, #=, N).
