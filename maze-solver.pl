:- module(mazeSolver, [solve/3]).
:- use_module('maze-utils.pl', [last_element/2, is_in/2]).
:- ensure_loaded(['maze.pl']).

available_move([X0,Y0], [X1,Y1]) :- 
	adj_tile([X0,Y0], [X1,Y1]), 
	available_tile(X0,Y0), 
	available_tile(X1,Y1).

available_tile(X0,Y0) :- 
	\+ barrier(X0,Y0), 
	\+ outside(X0,Y0).

outside(X1,Y1) :- 
	mazeSize(A,B), 
	((X1<1);
	(X1>A); 
	(Y1<1); 
	(Y1>B)).

adj_tile([X0,Y0], [X0,Y1]) :- 
	(Y1 is Y0+1); (Y1 is Y0-1).
adj_tile([X0,Y0], [X1,Y0]) :- 
	(X1 is X0+1); (X1 is X0-1).

solve(ST, ET, Path) :-
	start_tile_valid(ST, Path),
	path_solver(ST, ET, [], Path).
	%% \+ exists_shorter_path(A, B, Path).

start_tile_valid(ST, Path) :-
	Path = [ST|_].

path_solver(CurrentT, ET, Cumu, Path) :-
	CurrentT = ET,
	Cumu = Path.

path_solver(CurrentT, ET, Cumu, Path) :-
	available_move(CurrentT, D),
	\+ is_in(D, Cumu),
	path_solver(D, ET, [D|Cumu], Path).

%% exists_shorter_path(A, B, Path) :-
%% 	endpoints_valid(A, B, OPath),
%% 	path_valid([], OPath),
%% 	length(OPath, N),
%% 	length(Path, M),
%% 	(N < M),
%% 	!.



