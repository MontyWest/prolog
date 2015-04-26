:- module(maze, [maze_size/2, barrier/2]).

maze_size(5, 9).

barrier(1, 8).
barrier(2, 1).
barrier(2, 2).
barrier(2, 4).
barrier(2, 5).
barrier(3, 4).
barrier(3, 7).
barrier(3, 9).
barrier(4, 4).
barrier(4, 7).
barrier(4, 8).
barrier(4, 9).
barrier(5, 2).

%%    1 2 3 4 5 6 7 8 9
%%   +-----------------+
%% 1 |. . . . . . . x .|
%% 2 |x x . x x . . . .|
%% 3 |. . . x . . x . x|
%% 4 |. . . x . . x x x|
%% 5 |. x . . . . . . .|
%%   +-----------------+