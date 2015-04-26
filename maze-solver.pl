:- module(mazeSolver, [solve/3]).
:- use_module('maze-printer.pl', [print_maze/0, print_maze/1]).

%% True if moving from [X0, Y0] to [X1, Y1] is valid
available_move([X0,Y0], [X1,Y1]) :- 
	adj_tile([X0,Y0], [X1,Y1]), 
	%% available_tile(X0,Y0), 
	available_tile(X1,Y1).

%% True if tile is inside the maze
inside_maze(X1,Y1) :- 
	maze_size(A,B), 
	X1>=1,
	X1=<A,
	Y1>=1, 
	Y1=<B.

%% True tile can be used in a path
available_tile([X,Y]) :-
	available_tile(X,Y).
available_tile(X0,Y0) :- 
	inside_maze(X0,Y0),
	\+ barrier(X0,Y0).

%% True if [X0, Y0] is adajacent to [X1, Y1]
adj_tile([X0,Y0], [X0,Y1]) :- 
	(Y1 is Y0-1).
adj_tile([X0,Y0], [X1,Y0]) :- 
	(X1 is X0-1).
adj_tile([X0,Y0], [X0,Y1]) :- 
	(Y1 is Y0+1).
adj_tile([X0,Y0], [X1,Y0]) :- 
	(X1 is X0+1).


%%%%%% New %%%%%%

%% Checks endpoints first, then gets list of paths, then choses shortest from that list.
%% Cut after getting path list prevents back tracking after displaying all shortest lists
solve(ST, ET, Path) :-
	available_tile(ST),
	available_tile(ET),
	maze_size(N,M),
	get_paths_list(ST, ET, N*M, [], PathList),
	!,
	get_shortest_path(Path, PathList),
	print_maze(Path).

%% Plucks a path from list, if its length is minimal then true
get_shortest_path(Path, PathList) :-
	get_shortest_path_length(PathList, Min),
	member(Path, PathList),
	length(Path, Min).

%% Gets the minimum of the lengths of paths in the list (True if Min is the minimum length)
get_shortest_path_length([H|T], Min) :-
	length(H, MinStart),
	get_shortest_path_length(T, MinStart, Min).

%% Recurses through the list, taking the minimum length that it finds as it goes
get_shortest_path_length([], Min, Min).
get_shortest_path_length([NewPath|T], LatestMin, Min) :-
	length(NewPath, N),
	NewMin is min(N, LatestMin), 
	get_shortest_path_length(T, NewMin, Min).

%% recursively finds paths that satisify, when all are found
%% this returns false, and will move to next rule.
get_paths_list(ST, ET, MaxLength, Cumu, PathList) :-
	path_solver_plus(ST, ET, MaxLength, [ET], Path),
	length(Path, N),
	N =< MaxLength, %% redundant check assuming it works in the path solver
	\+ memberchk(Path, Cumu),
	get_paths_list(ST, ET, N, [Path|Cumu], PathList).
%% Order means this is only called when above rule fails (all paths are found)
%% acts as boundary condition for recursion.
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
	path_solver_plus(ST, D, MaxLength, [D|Cumu], Path).




%% OLDER LESS EFFICIENT %%

%% solve(ST, ET, Path) :-
%% 	available_tile(ST),
%% 	available_tile(ET),
%% 	path_solver(ST, ET, [ET], Path),
%% 	\+ exists_shorter_path(ST, ET, Path),
%% 	printMaze(Path).

%% %% Builds path backwards, path goes CurrentT -> ST
%% path_solver(ST, ST, Path, Path):-
%% 	!.
%% path_solver(ST, CurrentT, Cumu, Path) :-
%% 	available_move(CurrentT, ST),
%% 	Path = [ST|Cumu],
%% 	!.
%% path_solver(ST, CurrentT, Cumu, Path) :-
%% 	available_move(CurrentT, D),
%% 	\+ memberchk(D, Cumu),
%% 	path_solver(ST, D, [D|Cumu], Path).

%% exists_shorter_path(ST, ET, Path) :-
%% 	path_solver(ST, ET, [ET], OPath),
%% 	length(OPath, N),
%% 	length(Path, M),
%% 	(N < M),
%% 	!.
