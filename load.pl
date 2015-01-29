%% :- working_directory(_, 'Users/montywest/BBK/PPL/Prolog/Coursework').

:- set_prolog_flag(toplevel_print_options, [quoted(true), portray(true), max_depth(50), spacing(next_argument)]).

:- abolish(barrier/2).
:- abolish(mazeSize/2).

:- use_module('maze-big.pl', [mazeSize/2, barrier/2]).
%% :- use_module('maze.pl', [mazeSize/2, barrier/2]).
:- use_module('grid-printer.pl', [printGrid/1]).
:- use_module('maze-solver.pl', [solve/3]).

