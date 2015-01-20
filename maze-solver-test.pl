:- ensure_loaded(['maze-solver.pl']).

:- mazeSolver:adj_tile([2,1], [2,2]).
:- mazeSolver:adj_tile([3,2], [3,1]).
:- mazeSolver:adj_tile([5,5], [6,5]).
:- mazeSolver:adj_tile([4,3], [3,3]).
:- \+ mazeSolver:adj_tile([2,1], [1,2]).