:- module(mazePrinter, [printMaze/0, printMaze/1]).

printMaze :-
	printMaze([]).
printMaze(Path) :-
	mazeSize(Vert, Hori), nl,
	printHeader(1,Hori), nl,
	printMazeHorizEdge(1,Hori), nl,
	printInnerMaze(1, Vert, Hori, Path), nl,
	printMazeHorizEdge(1,Hori).

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
	From>=To.
printColumnHeader(From, To) :-
	write(From),
	printHeadingInnerSpacer(From),
	Next is From+1,
	printColumnHeader(Next, To),
	!.

printMazeHorizEdge(From, To) :-
	printSpacerCorner,
	write('+'),
	printMazeHorizBorder(From, To),
	write('+').

printMazeHorizBorder(To, To) :-
	write('---').
printMazeHorizBorder(From, To) :-
	From>=To.	
printMazeHorizBorder(From, To) :-
	write('--'),
	Next is From+1,
	printMazeHorizBorder(Next, To),
	!.

printInnerMaze(Row, Row, Columns, Path) :-
	printMazeRow(Row, 1, Columns, Path).
printInnerMaze(Row, RowTo, _, _) :-
	Row>=RowTo.
printInnerMaze(Row, RowTo, Columns, Path) :-
	printMazeRow(Row, 1, Columns, Path),
	nl,
	RowNext is Row+1,
	printInnerMaze(RowNext, RowTo, Columns, Path),
	!.

printHeadingInnerSpacer(Row) :-
	Row > 9 ; write(' '),
	true.

printMazeRow(Row, ColumnFrom, ColumnTo, Path) :-
	printSpacerRow,
	write(Row),
	printHeadingInnerSpacer(Row),
	write('|'),
	printMazeRowInner(Row, ColumnFrom, ColumnTo, Path).

printMazeRowInner(Row, Column, Column, Path):-
	write(' '),
	printSymbol(Row, Column, Path),
	write(' |').
printMazeRowInner(_, Column, ColumnTo, _) :-
	Column>=ColumnTo.
printMazeRowInner(Row, Column, ColumnTo, Path) :-
	write(' '),
	printSymbol(Row, Column, Path),
	NextColumn is Column+1,
	printMazeRowInner(Row, NextColumn, ColumnTo, Path)
	,!.

printSymbol(Row, Column, Path) :-
	memberchk([Row,Column], Path), write('o');
	barrier(Row, Column), write('x');
	write('.').



