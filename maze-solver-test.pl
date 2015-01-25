:- ensure_loaded(['maze-solver.pl']).

:- mazeSolver:available_move([1,7], [1,6]).
:- mazeSolver:available_move([1,7], [2,7]).
:- \+ mazeSolver:available_move([1,7], [1,8]).
:- \+ mazeSolver:available_move([1,7], [0,7]).
:- \+ mazeSolver:available_move([1,7], [2,8]).
:- \+ mazeSolver:available_move([1,7], [1,9]).

:- mazeSolver:outside(0, 3).
:- mazeSolver:outside(8, -2).
:- mazeSolver:outside(10000, 3).
:- mazeSolver:outside(1, 10000).
:- \+ mazeSolver:outside(2,2).

:- mazeSolver:adj_tile([2,1], [2,2]).
:- mazeSolver:adj_tile([3,2], [3,1]).
:- mazeSolver:adj_tile([5,5], [6,5]).
:- mazeSolver:adj_tile([4,3], [3,3]).
:- \+ mazeSolver:adj_tile([2,1], [1,2]).

:- mazeSolver:endpoints_valid([2,3], [3,4], [[2,3],[3,3],[3,4]]).
:- \+ mazeSolver:endpoints_valid([2,3], [3,4], [[5,6],[6,6],[6,7]]).

:- mazeSolver:path_valid([[1,6],[2,6],[3,6],[3,5]]).


