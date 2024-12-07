:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(reif)).
 
reports([X|Xs]) --> report(X), ['\n'], reports(Xs).
reports([])     --> [].

report([L|Ls]) --> integer(L), whitespace, report(Ls).
report([L])    --> integer(L).

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

% A Report is safe if it is either monotonically increasing or decreasing.
safe(Report) :- increasing(Report).
safe(Report) :- decreasing(Report).

% True iff Ls is monotonically increasing, and adjacent levels differ by at least one or at most
% three.
increasing([]).
increasing([_]).
increasing([X,Y|Xs]) :-
	Y #> X,
	abs(X - Y) #= D,
	D in 1..3,
	increasing([Y|Xs]).

% True iff Ls is monotonically decreasing, and adjacent levels differ by at least one or at most
% three.
decreasing([]).
decreasing([_]).
decreasing([X,Y|Xs]) :-
	Y #< X,
	abs(X - Y) #= D,
	D in 1..3,
	decreasing([Y|Xs]).

% N is the number of safe reports in File.
part_1(File, N) :-
	once(phrase_from_file(reports(Reports), File)),
	findall(Report, (member(Report, Reports), safe(Report)), Safe),
	length(Safe, N).

dampen(Report, Safe) :-
	select(_, Report, Safe),
	safe(Safe).

% N is the number of safe reports in File after using the Problem Dampener.
part_2(File, N) :-
	once(phrase_from_file(reports(Reports), File)),
	findall(Report, (member(Report, Reports), once(dampen(Report, Safe))), Safe),
	length(Safe, N).
