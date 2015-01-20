is_in(X, [X|_]).
is_in(X,Z) :- Z = [_|Y], is_in(X,Y).

shortest(X, List) :- 
	is_in(X, List),
	\+ is_shorter(X, List).

is_shorter(X, List) :-
	is_in(Y, List),
	Y < X, !.