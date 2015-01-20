
available_move([X0,Y0], [X1,Y1]) :- 
	adj_tile([X0,Y0], [X1,Y1]), 
	\+ barrier(X1,Y1), 
	\+ outside(X1,Y1).

outside(X1,Y1) :- 
	mazeSize(A,B), 
	(X1<1), 
	(X1>A), 
	(Y1<1), 
	(Y1>B).

adj_tile([X0,Y0], [X0,Y1]) :- 
	(Y1 is Y0+1); (Y1 is Y0-1).
adj_tile([X0,Y0], [X1,Y0]) :- 
	(X1 is X0+1); (X1 is X0-1).

solve([X0,Y0], [Xn,Yn], Path) :-
	solve([X0, Y0], [Xn, Yn], OPath),
	length(Path, A),
	length(OPath, B),
	(A <= B).