:-op(140, fy, -).	% stands for 'not'
:-op(160,xfy, [and, or, equiv, imp, impinv, nand, nor, nonimp, nonequiv, nonimpinv]).

is_true(V, X and Y) :- is_true(V,X), is_true(V,Y).
is_true(V, X or _) :- is_true(V,X).
is_true(V, _ or Y) :- is_true(V,Y).
is_true(V, -X) :-
	not(is_true(V, X)). % link with Prolog's negation
is_true(v0,a).	% this means that v0 sends a to True and everything else (here, b and c) to false

is_true(V, X) :-
	member(X,V).	% only true elements are explicitly mentioned in V

%Implication and equivalence

is_true(V, X imp Y) :- is_true(V, (-X) or Y).
is_true(V, X equiv Y) :- 
    is_true(V, X imp Y), 
    is_true(V, Y imp X).


valuation(V) :-
	% we keep all elements that V sends to true.
	% all other elements are supposed to be false.
	sub_set(V, [a,b,c]).	
	
sub_set([], []).
sub_set([X|XL], [X|YL]) :-
    sub_set(XL, YL).
sub_set(XL, [_|YL]) :-
    sub_set(XL, YL).


% f1 :- (a imp (b imp c)) imp ((a imp b) imp (a imp c )).
% f2 :- ((a imp b) and (b imp c)) imp -(-c and a).

/* table for unary, alpha and beta formulas */

components(- -X, X, _, unary).
components(X and Y, X, Y, alpha).
components(-(X or Y), -X, -Y, alpha).
components(X or Y, X, Y, beta).
components(-(X and Y), -X, -Y, beta).
components(X imp Y, -X, Y, beta).
components(-(X imp Y), X, -Y, alpha).
components(X impinv Y, X, -Y, beta).
components(-(X impinv Y), -X, Y, alpha).
components(X nand Y, -X, -Y, beta).
components(-(X nand Y), X, Y, alpha).
components(X nor Y, -X, -Y, alpha).
components(-(X nor Y), X, Y, beta).
components(X nonimp Y, X, -Y, alpha).
components(-(X nonimp Y), -X, Y, beta).
components(X nonimpinv Y, -X, Y, alpha).
components(-(X nonimpinv Y), X, -Y, beta).


% Predicate cnf puts more elementary processing together
cnf(Conjunction, NewConjunction) :-
	oneStep(Conjunction, C1),
	cnf(C1, NewConjunction).
cnf(C, C).


% Predicate oneStep performs one elementary processing
oneStep([Clause | Rest_Conjonction], [ [F1, F2 | Rest_Clause] | Rest_Conjonction]) :-
	% looking for a beta formula in the clause
	remove(F1 or F2, Clause, Rest_Clause).

% Transition for double negation
oneStep([Clause | Rest_Conjonction], [[Pos| Rest_Clause] | Rest_Conjonction]):-
    components(Double_neg, Pos, _, unary),
    remove(Double_neg, Clause, Rest_Clause).

%Transition for alpha clauses
oneStep([Clause | Rest_Conjonction], [[Alpha_1 | Rest_Clause], [Alpha_2 | Rest_Clause] | Rest_Conjonction]):-
    remove(Alpha_formula, Clause, Rest_Clause),
    components(Alpha_formula, Alpha_1, Alpha_2, alpha).

%Transition for beta clauses
oneStep([Clause | Rest_Conjonction], [[Beta_1, Beta_2 | Rest_Clause] | Rest_Conjonction]):-
    remove(Beta_formula, Clause, Rest_Clause),
    components(Beta_formula, Beta_1, Beta_2, beta).
    
oneStep([ F | Rest], [ F | New_Rest ]) :-
	% nothing left to do on F
	oneStep(Rest, New_Rest).

/*------------------------------------------------*/
/* Auxiliary predicates                           */
/*------------------------------------------------*/

/* remove does as select, but removes all occurrences of X */
remove(X, L, NL) :-
    member(X,L),	% so that remove fails if X absent from L
    remove1(X, L, NL).
remove1(X, L, L) :-
    not(member(X,L)).
remove1(X, L, NL) :-
	select(X, L, L1),   % available in SWI-Prolog
	remove1(X, L1, NL).

prove(F) :-
    cnf([[ -F ]], CNF),
    write('CNF of -'), write(F), write(' = '),
    write(CNF), nl,
    resolve(CNF).


resolve(CNF) :-
    member([ ], CNF),
    write('This is a true formula'), nl.

resolve(CNF) :-
    write('Examining '), write(CNF), nl,
    get0(_),    % waits for user action
    select(C1, CNF, _),            % forgetting this parent clause
    select(C2, CNF, RCNF),    % keeping this parent clause
    select(P, C1, RestC1),
    select(-P, C2, RestC2),
    resolve([RestC1, RestC2|RCNF]).
    

go :- prove(((a(X) imp (b imp c)) imp ((a(Y) imp b) imp (a(3) imp c)))).