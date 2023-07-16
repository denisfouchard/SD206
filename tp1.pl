% SD-TSIA 206
% TP1 - Denis Fouchard 

% Define Simpson Family

parent(marge, lisa).
parent(marge, bart).
parent(marge, maggie).
parent(homer, lisa).
parent(homer, bart).
parent(homer, maggie).
parent(abraham, homer).
parent(abraham, herb).
parent(mona, homer).
parent(jackie, marge).
parent(clancy, marge).
parent(jackie, patty).
parent(clancy, patty).
parent(jackie, selma).
parent(clancy, selma).
parent(selma, ling).

female(mona).
female(jackie).
female(marge).
female(ann).
female(patty).
female(selma).
female(ling).
female(lisa).
female(maggie).
male(abraham).
male(herb).
male(homer).
male(bart).
male(clancy).

child(X,Y) :-
    parent(Y,X).

mother(X,Y) :-
    parent(X,Y),
    female(X).

grandparent(X,Y) :-
    parent(X,Z), % note that the a variable s scope is the clause
    parent(Z,Y). % variable Z keeps its value within the clause

sister(X,Y) :-
    parent(Z,X), % if X gets instantiated, Z gets instantiated as well
    parent(Z,Y),
    female(X),
    X \== Y. % can also be noted: not(X = Y).

ancestor(X,Y) :-
    parent(X,Y).
ancestor(X,Y) :-
    parent(X,Z),
    ancestor(Z,Y). % recursive call
    
% Check if X is aunt of Y
aunt(X,Y) :- 
    sister(X,S), 
    parent(S,Y).



% List extraction
extract(X, [X|L], L).
extract(X, [Y|L], [Y|L1]) :-
    extract(X, L, L1).

% Perumation
permute([],[]).
permute([First|Rest], PermutedList) :- 
    permute(Rest, PermutedRest),
    extract(First, PermutedList, PermutedRest).

last_elt([X|[]], X).
last_elt([_X|L], LastElt) :- last_elt(L, LastElt).

% Attach
attach([], L2, L2).
attach([X|Q], L2, [X|JoinedList]) :- attach(Q, L2, JoinedList).

% Concatenation
assemble(L1, L2, L3, Res) :-
    attach(L1, L2, L4),
    attach(L4, L3, Res).

% sub_list
sub_list([], []).
sub_list(IncludedList, IncludingList) :- 
    assemble(_L3, IncludedList, _L4, IncludingList).

% Perec
remove(_X, [], []).
remove(X, [X|L1], L2) :- remove(X, L1, L2),!.
remove(X, [Y|L1], [Y|L2]) :- 
    remove(X, L1, L2).

% Duplicate
duplicate([], []).
duplicate([X|Y], [X,X|L]) :- duplicate(Y, L).

% duplicate elements of a list
duplicate_list([], []).
duplicate_list([X|Y], [X,X|L]) :- duplicate_list(Y, L).

% duplicate elements of a list
duplicate_list2([], []).
duplicate_list2([X|Y], [X,X|L]) :- duplicate_list2(Y, L).

% invert a list
invert([], []).
invert([X|Y], L) :- invert(Y, L1), append(L1, [X], L).

