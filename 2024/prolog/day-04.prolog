:- use_module(library(between)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).

% Ls is a list of lists.
grid([L|Ls]) --> seq(L), ['\n'], grid(Ls).
grid([])     --> [].

% All possible ways to find XMAS in the top-left of a grid.
xmas([['X','M','A','S'|_]|_]).
xmas([['S','A','M','X'|_]|_]).
xmas([['X'|_],
	    ['M'|_],
	    ['A'|_],
	    ['S'|_]|_]).
xmas([['S'|_],
	    ['A'|_],
	    ['M'|_],
	    ['X'|_]|_]).
xmas([['X'      |_],
	    [_,'M'    |_],
	    [_,_,'A'  |_],
	    [_,_,_,'S'|_]|_]).
xmas([['S'      |_],
	    [_,'A'    |_],
	    [_,_,'M'  |_],
	    [_,_,_,'X'|_]|_]).
xmas([[_,_,_,'X'|_],
	    [_,_,'M'|_],
	    [_,'A'|_],
	    ['S'|_]|_]).
xmas([[_,_,_,'S'|_],
	    [_,_,'A'|_],
	    [_,'M'|_],
	    ['X'|_]|_]).

% All tails of a list
tails([], []).
tails([X|Xs], [[X|Xs]|Ys]) :- tails(Xs, Ys).

% The tail of a list.
tail([_|Xs], Xs).

% Count the number of ways to find XMAS in a grid without moving vertically.
count_xmas([[]|_], 0).
count_xmas(Rows, N) :-
	Rows = [_|_],
	findall(_, xmas(Rows), Rs),
	length(Rs, X),
	maplist(tail, Rows, Rows1),
	count_xmas(Rows1, Y),
	N #= X + Y.

% N in the number of ways to find XMAS in File.
part_1(File, N) :-
	once(phrase_from_file(grid(Rows), File)),
	tails(Rows, Rowss),
	maplist(count_xmas, Rowss, Ns),
	sum(Ns, #=, N).

% x-mas is true if the given grid starts with an X-MAS.
'x-mas'([[A ,  _  , B|_], 
		 [_ , 'A' , _|_], 
		 [C ,  _  , D|_]|_]) :-
	( A = 'S', D = 'M' ; A = 'M', D = 'S' ),
	( B = 'S', C = 'M' ; B = 'M', C = 'S' ).

% Count all horizontal x-mases
'count_x-mas'([[]|_], 0).
'count_x-mas'(Rows, N) :-
	Rows = [_|_],
	findall(_, 'x-mas'(Rows), Rs),
	length(Rs, X),
	maplist(tail, Rows, Rows1),
	'count_x-mas'(Rows1, Y),
	N #= X + Y.

% N in the number of ways to find an X-MAS in File.
part_2(File, N) :-
	once(phrase_from_file(grid(Rows), File)),
	tails(Rows, Rowss),
	maplist('count_x-mas', Rowss, Ns),
	sum(Ns, #=, N).
