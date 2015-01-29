:- module(mazeSolver, [solve/3]).
:- use_module('maze-utils.pl', [last_element/2, is_in/2]).
%% :- use_module('grid-printer.pl', [printGrid/1]).


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



%%%%%% New %%%%%%

%% Checks endpoints first, then gets list of paths, then choses from that list.
solve_shortest_eff(ST, ET, Path) :-
	available_tile(ST),
	available_tile(ET),
	mazeSize(N,M),
	get_paths_list(ST, ET, N*M, [], PathList),
	!,
	get_shortest_path(Path, PathList),
	printGrid(Path).

%% Plucks a path from list, if its length is minimal then returns
get_shortest_path(Path, PathList) :-
	get_shortest_path_length(PathList, Min),
	member(Path, PathList),
	length(Path, Min).

%% Gets the minimum of the lengths of paths in the list
get_shortest_path_length(PathList, Min) :-
	PathList = [H|T],
	length(H, MinStart),
	get_shortest_path_length(T, MinStart, Min).

%% Recurses through the list, taking the minimum length that it finds as it goes
get_shortest_path_length([], Min, Min).
get_shortest_path_length(PathList, LatestMin, Min) :-
	PathList = [NewPath|T],
	length(NewPath, N),
	NewMin is min(N, LatestMin), 
	get_shortest_path_length(T, NewMin, Min).

%% recursively finds paths that satisify, when all are found 
%% this returns will false, and move to next rule.
get_paths_list(ST, ET, MaxLength, Cumu, PathList) :-
	path_solver_plus(ST, ET, MaxLength, [ET], Path),
	length(Path, N),
	N < MaxLength, %% redundant check assuming it works in the path solver
	\+ memberchk(Path, Cumu),
	get_paths_list(ST, ET, MaxLength, [Path|Cumu], PathList).
%% Order means this is only called when above rule fails.
get_paths_list(_, _, _, PathList, PathList).

%% Solves for path backwards (as it's easy to build list that way)
%% When the current tile is the start tile then end.
path_solver_plus(ST, ST, _, Path, Path):-
	!.
%% Sink effect on the start tile, if we are currently one tile away then move straight there.
path_solver_plus(ST, CurrentT, MaxLength, Cumu, Path) :-
	available_move(CurrentT, ST),
	length(Cumu, N),
	N < MaxLength,
	Path = [ST|Cumu],
	!.
%% Finds an available move, if we haven't been there, add to the path and recurse
%% If the current path being built gets bigger than max allowed length then not allowed
path_solver_plus(ST, CurrentT, MaxLength, Cumu, Path) :-
	length(Cumu, N),
	N < MaxLength,
	available_move(CurrentT, D),
	\+ memberchk(D, Cumu),
	path_solver(ST, D, [D|Cumu], Path).


