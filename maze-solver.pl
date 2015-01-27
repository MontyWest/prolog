:- module(mazeSolver, [solve/3, printGrid/1]).
:- use_module('maze-big.pl', [mazeSize/2, barrier/2]).
:- use_module('maze-utils.pl', [last_element/2, is_in/2]).
:- use_module('grid-printer.pl', [printGrid/1]).


available_move([X0,Y0], [X1,Y1]) :- 
	adj_tile([X0,Y0], [X1,Y1]), 
	available_tile(X0,Y0), 
	available_tile(X1,Y1).

inside_maze(X1,Y1) :- 
	mazeSize(A,B), 
	X1>=1,
	X1=<A,
	Y1>=1, 
	Y1=<B.

available_tile([X,Y]) :-
	available_tile(X,Y).
available_tile(X0,Y0) :- 
	inside_maze(X0,Y0),
	\+ barrier(X0,Y0).


adj_tile([X0,Y0], [X0,Y1]) :- 
	(Y1 is Y0-1).
adj_tile([X0,Y0], [X1,Y0]) :- 
	(X1 is X0-1).
adj_tile([X0,Y0], [X0,Y1]) :- 
	(Y1 is Y0+1).
adj_tile([X0,Y0], [X1,Y0]) :- 
	(X1 is X0+1).

solve(ST, ET, Path) :-
	available_tile(ST),
	available_tile(ET),
	path_solver(ST, ET, [ET], Path),
	\+ exists_shorter_path(ST, ET, Path),
	printGrid(Path).


%% Builds path backwards, path goes CurrentT -> ST
path_solver(ST, ST, Path, Path):-
	!.
path_solver(ST, CurrentT, Cumu, Path) :-
	available_move(CurrentT, ST),
	Path = [ST|Cumu],
	!.
path_solver(ST, CurrentT, Cumu, Path) :-
	available_move(CurrentT, D),
	\+ memberchk(D, Cumu),
	path_solver(ST, D, [D|Cumu], Path).

exists_shorter_path(ST, ET, Path) :-
	path_solver(ST, ET, [ET], OPath),
	length(OPath, N),
	length(Path, M),
	(N < M),
	!.



