:- ensure_loaded(['maze.pl']).

printGrid(Path) :-
	mazeSize(Vert, Hori), nl,
	printHeader(1,Hori), nl,
	printGridHorizEdge(1,Hori), nl,
	printInnerGrid(1, Vert, Hori, Path), nl,
	printGridHorizEdge(1,Hori).

printSpacerTop :-
	write('     '). %%5

printSpacerCorner :-
	write('   '). %%3

printSpacerRow :-
	write(' '). %%1

printHeader(From, To) :-
	printSpacerTop,
	printColumnHeader(From, To).

printColumnHeader(To, To) :-
	write(To).
printColumnHeader(From, To) :-
	write(From),
	write(' '),
	Next is From+1,
	printColumnHeader(Next, To).

printGridHorizEdge(From, To) :-
	printSpacerCorner,
	write('+'),
	printGridHorizBorder(From, To),
	write('+').

printGridHorizBorder(To, To) :-
	write('---').
printGridHorizBorder(From, To) :-
	write('--'),
	Next is From+1,
	printGridHorizBorder(Next, To).

printInnerGrid(Row, Row, Columns, Path) :-
	printGridRow(Row, 1, Columns, Path).

printInnerGrid(Row, RowTo, Columns, Path) :-
	printGridRow(Row, 1, Columns, Path),
	nl,
	RowNext is Row+1,
	printInnerGrid(RowNext, RowTo, Columns, Path).

printGridRow(Row, ColumnFrom, ColumnTo, Path) :-
	printSpacerRow,
	write(Row),
	write(' |'),
	printGridRowInner(Row, ColumnFrom, ColumnTo, Path).

printGridRowInner(Row, Column, Column, Path):-
	write(' '),
	printSymbol(Row, Column, Path),
	write(' |').
printGridRowInner(Row, Column, ColumnTo, Path) :-
	write(' '),
	printSymbol(Row, Column, Path),
	NextColumn is Column+1,
	printGridRowInner(Row, NextColumn, ColumnTo, Path).

printSymbol(Row, Column, Path) :-
	memberchk([Row,Column], Path), write('o');
	barrier(Row, Column), write('x');
	write('.').



