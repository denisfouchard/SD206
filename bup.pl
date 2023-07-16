/*---------------------------------------------------------------*/
/* Telecom Paris- J-L. Dessalles 2023                            */
/* LOGIC AND KNOWLEDGE REPRESENTATION                            */
/*            http://teaching.dessalles.fr/LKR                   */
/*---------------------------------------------------------------*/


	%%%%%%%%%%%%%
	% bottom-up recognition  %
	%%%%%%%%%%%%%

:- consult('dcg2rules.pl').     % DCG to 'rule' converter: np --> det, n. becomes rule(gn, [det, n])
:- dcg2rules('family.pl').      % performs the conversion by asserting rule(np, [det, n])

bup([s]) :-
	writeln([s]).  % success when one gets s after a sequence of transformations
bup(P):-
	write(P), get0(_),
	append(Pref, Rest, P),   % P is split into three pieces 
	append(RHS, Suff, Rest), % P = Pref + RHS + Suff
	rule(X, RHS),	% bottom up use of rule
	append(Pref, [X|Suff], NEWP),  % RHS is replaced by X in P:  NEWP = Pref + X + Suff
	bup(NEWP).  % lateral recursive call

go :-
	Sentence = [the, sister, talks, about, her, cousin],
	writeln(Sentence),
	bup(Sentence).
