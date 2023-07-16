/*---------------------------------------------------------------*/
/* Telecom Paris- J-L. Dessalles 2023                            */
/* LOGIC AND KNOWLEDGE REPRESENTATION                            */
/*            http://teaching.dessalles.fr/LKR                   */
/*---------------------------------------------------------------*/


% adapted from I. Bratko - "Prolog - Programming for Artificial Intelligence"
%              Addison Wesley 1990

% An ape is expected to form a plan to grasp a hanging banana using a box.
% Possible actions are 'walk', 'climb (on the box)', 'push (the box)', 
% 'grasp (the banana)'

% description of actions - The current state is stored using a functor 'state'
% with 4 arguments: 
%	- horizontal position of the ape 
%	- vertical position of the ape
%	- position of the box
%	- status of the banana 
% 'action' has three arguments: 
% 	- Initial state
%	- Final state
%	- act


% LE PLUS DE CONTRAINTES AU DEBUT.

action(state(middle,on_box,X,not_holding), grasp, state(middle,on_box,X,holding)).
action(state(X,floor,X,Y), climb, state(X,on_box,X,Y)).
action(state(X,floor,X,Z), push(X,Y), state(Y,floor,Y,Z)).
action(state(X,floor,T,Z), walk(X,Y), state(Y,floor,T,Z)).

success(state(_,_, _, holding), []).
success(State1, [Act|Plan]) :- 
	action(State1, Act, State2),
	success(State2, Plan).

go :- success(state(door, floor, window, not_holding), P).


% bad solution (double recursion):
mirror([ ], [ ]).
mirror([X|L1], L2) :-
    mirror(L1,L3),
    append(L3, [X], L2).     % append will dig into the list a second time


% better solution with accumulator:
mirror2(Left, Right) :-
    invert(Left, [ ], Right).
invert([X|L1], L2, L3) :-    % the list is 'poured'
    invert(L1, [X|L2], L3).    % into the second argument
invert([ ], L, L).        % at the deepest level, the result L is merely copied


invert2([], L2).
invert2([X|L1], L2) :- invert2(L1, [X|L2]).


% Naive palindrome (my version)
palindrome(L1) :- 
    mirror2(L1, L1_r),
    L1_r = L1.

% Better naive palindrome
palindrome_(L1) :- mirror2(L1,L1). 

% Palindrome with accumulation
palindrome_aux(L, L) :-
    !.
palindrome_aux(L, [_|L]) :-
    !.
palindrome_aux([X|L1], R) :- 
    palindrome_aux(L1, [X|R]).

palindrome_acc(L) :- palindrome_aux(L, []).

% Retractall

:-dynamic(well_known/1).

/*
assertz(well_known(katy)).
assertz(well_known('Elvis')).
assertz(well_known(madonna)).
assertz(well_known(michael)).
*/

empty(P) :-
    retract(P).    % this will force backtracking
empty(_).    % terminate with success  


% Find any elements

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

:-dynamic(found/1).

collect_found([X|Results]) :- 
    found(X),
    retract(found(X)),
    collect_found(Results),
    !.
    

collect_found([]).

findany(Var, Pred, Results) :- 
    Pred,
    not(found(Var)),
    asserta(found(Var)),
    fail,
    write(Var),
    findany(Var, Pred, Results).



findany(_, _, Results) :- 
    collect_found(Results).


%--------------------------------
%       semantic networks
%--------------------------------

isa(bird, animal).
isa(albert, albatross).
isa(albatross, bird).
isa(kiwi, bird).
isa(willy, kiwi).
isa(crow, bird).
isa(ostrich, bird).

:- dynamic(locomotion/2).	% for tests

locomotion(bird, fly).
locomotion(tiger, walk).
locomotion(kiwi, walk).
locomotion(X, Loc) :-
	isa(X, SuperX),
	locomotion(SuperX, Loc).

food(albatross,fish).
food(bird,grain).


/* drawback : n particular inheritance rules */
/* solution: one general predicate : "known" */

known(Fact) :- 
	Fact,
	!.
known(Fact) :-
	Fact =.. [Rel, Arg1, Arg2],
	isa(Arg1, SuperArg1),
	SuperFact =.. [Rel, SuperArg1, Arg2],
	known(SuperFact).


habitat(Animal, continent) :-
    known(locomotion(Animal, X)),
    X \== fly,
    !.

habitat(_, unknown).

