:- module(mazeUtils, [reverse/2]).

reverse([], []).
reverse([H|T], RevList):-
	reverse(T, RevT),
	append(RevT, [H], RevList).

