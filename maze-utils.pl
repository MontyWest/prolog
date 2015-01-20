:- module(mazeUtils, [reverse/2]).

reverse([], []).
reverse([H|T], RevList):-
	reverse(T, RevT),
	conc(RevT, [H], RevList).