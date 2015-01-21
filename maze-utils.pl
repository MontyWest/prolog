:- module(mazeUtils, [reverse/2, last_element/2, is_in/2]).

reverse([], []).
reverse([H|T], RevList):-
	reverse(T, RevT),
	append(RevT, [H], RevList).

last_element([G], G).
last_element(List, G) :-
	List = [_|T],
	last_element(T, G).

is_in(X, [X|_]).
is_in(X,Z) :- Z = [_|Y], is_in(X,Y).