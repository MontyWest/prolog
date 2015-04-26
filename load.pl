:- set_prolog_flag(toplevel_print_options, [quoted(true), portray(true), max_depth(50), spacing(next_argument)]).

:- abolish(barrier/2).
:- abolish(maze_size/2).

:- use_module('maze-big.pl', [maze_size/2, barrier/2]).
%% :- use_module('maze-bad.pl', [maze_size/2, barrier/2]).
%% :- use_module('maze.pl', [maze_size/2, barrier/2]).
:- use_module('maze-printer.pl', [print_maze/0, print_maze/1]).
:- use_module('maze-solver.pl', [solve/3]).
