:- module(mazePrinter, [print_maze/0, print_maze/1]).

print_maze :-
	print_maze([]).
print_maze(Path) :-
	maze_size(Vert, Hori), nl,
	print_header(1,Hori), nl,
	print_maze_horiz_edge(1,Hori), nl,
	print_inner_maze(1, Vert, Hori, Path), nl,
	print_maze_horiz_edge(1,Hori).

print_spacer_top :-
	write('     '). %%5

print_spacer_corner :-
	write('   '). %%3

print_spacer_row :-
	write(' '). %%1

print_header(From, To) :-
	print_spacer_top,
	print_column_header(From, To).

%% Prints column header until false, hence the need for the cut
%% which prevents back tracking after header is printed
print_column_header(To, To) :-
	write(To).
print_column_header(From, To) :-
	From>=To.
print_column_header(From, To) :-
	write(From),
	print_heading_inner_spacer(From),
	Next is From+1,
	print_column_header(Next, To),
	!.

print_maze_horiz_edge(From, To) :-
	print_spacer_corner,
	write('+'),
	print_maze_horiz_border(From, To),
	write('+').

%% Prints top/bottom border until false, hence the need for the cut
%% which prevents back tracking after border is printed
print_maze_horiz_border(To, To) :-
	write('---').
print_maze_horiz_border(From, To) :-
	From>=To.	
print_maze_horiz_border(From, To) :-
	write('--'),
	Next is From+1,
	print_maze_horiz_border(Next, To),
	!.

%% Prints the inside of the maze (below top border, above bottom border)
%% Cut prevents back tracking when mze is printed
print_inner_maze(Row, Row, Columns, Path) :-
	print_maze_row(Row, 1, Columns, Path).
print_inner_maze(Row, RowTo, _, _) :-
	Row>=RowTo.
print_inner_maze(Row, RowTo, Columns, Path) :-
	print_maze_row(Row, 1, Columns, Path),
	nl,
	RowNext is Row+1,
	print_inner_maze(RowNext, RowTo, Columns, Path),
	!.

print_heading_inner_spacer(Row) :-
	Row > 9 ; write(' '),
	true.

print_maze_row(Row, ColumnFrom, ColumnTo, Path) :-
	print_spacer_row,
	write(Row),
	print_heading_inner_spacer(Row),
	write('|'),
	print_maze_row_inner(Row, ColumnFrom, ColumnTo, Path).

%% Prints a row of the inner maze, stops when it reachs maze size
%% Cute prevents backtracking after row is printed
print_maze_row_inner(Row, Column, Column, Path):-
	write(' '),
	print_symbol(Row, Column, Path),
	write(' |').
print_maze_row_inner(_, Column, ColumnTo, _) :-
	Column>=ColumnTo.
print_maze_row_inner(Row, Column, ColumnTo, Path) :-
	write(' '),
	print_symbol(Row, Column, Path),
	NextColumn is Column+1,
	print_maze_row_inner(Row, NextColumn, ColumnTo, Path),
	!.

%% Prints 'o' for tile in the path
%% Prints 'x' for a barrier
%% Prints '.' otherwise
print_symbol(Row, Column, Path) :-
	memberchk([Row,Column], Path), write('o').
print_symbol(Row, Column, _) :-
	barrier(Row, Column), write('x').
print_symbol(_, _, _) :-
	write('.').



