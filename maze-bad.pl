:- module(mazeBad, [barrier/2]).

barrier(X, Y) :-
 0 is X mod 2,
 0 is Y mod 2.