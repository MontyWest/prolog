:- ensure_loaded(['maze-solver.pl']).

:- mazeSolver:outside(0, 3).
:- mazeSolver:outside(8, -2).
:- mazeSolver:outside(10000, 3).
:- mazeSolver:outside(1, 10000).

:- mazeSolver:adj_tile([2,1], [2,2]).
:- mazeSolver:adj_tile([3,2], [3,1]).
:- mazeSolver:adj_tile([5,5], [6,5]).
:- mazeSolver:adj_tile([4,3], [3,3]).
:- \+ mazeSolver:adj_tile([2,1], [1,2]).

:- mazeSolver:endpoints_valid([2,3], [3,4], [[2,3],[3,3],[3,4]]).