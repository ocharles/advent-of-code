:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).

% Find all non-corrupted muls().
muls([X|Xs]) --> mul(X), muls(Xs).
muls(Xs)     --> [_], muls(Xs).
muls([])     --> [].

% mul(X-Y) parses the string "mul(X, Y)"
mul(X-Y) --> "mul(", integer(X), ",", integer(Y), ")".

% N in an integer, parsed as a sequence of ASCII digits.
integer(N) --> digits(Ds), { number_chars(N, Ds) }.

% A run of one or more ASCII digits.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D])    --> digit(D).

% D is a single ASCII digit.
digit(D) --> [D], { char_code('0', Zero), char_code(D, Code), Code >= Zero, Code =< Zero + 9 }.

% Z is the product of the pair X-Y.
mul(X-Y, Z) :- Z #= X * Y.

part_1(File, N) :-
	once(phrase_from_file(muls(Xs), File)),
	maplist(mul, Xs, Ys),
	sum(Ys, #=, N).

% Parses "don't()".
dont --> "don't()".

% Skips zero or more characters until the string "do()".
find_do --> "do()".
find_do --> [_], find_do.

% If the string starts with "don't()", skip until the next "do()" and then find muls.
enabled_muls(Xs)     --> dont, find_do, enabled_muls(Xs).

% Find muls, as before.
enabled_muls([X|Xs]) --> mul(X), enabled_muls(Xs).
enabled_muls(Xs)     --> [_], enabled_muls(Xs).
enabled_muls([])     --> [].

part_2(File, N) :-
	once(phrase_from_file(enabled_muls(Xs), File)),
	maplist(mul, Xs, Ys),
	sum(Ys, #=, N).
