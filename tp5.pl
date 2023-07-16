/*---------------------------------------------------------------*/
/* Telecom Paris- J-L. Dessalles 2023                            */
/* LOGIC AND KNOWLEDGE REPRESENTATION                            */
/*            http://teaching.dessalles.fr/LKR                   */
/*---------------------------------------------------------------*/



/**************************************************************/
/*  Simulation du jeu de Marienbad                            */
/**************************************************************/

sauve :-
	qsave_program('marienb.exe',[goal=go,autoload=true]).

%%%%%%%%%%%%%%%%%%%%%%%%%
% marienbad game (Nim)  %
%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic silent/0, value/2.

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Players take as many items they want in a single row
          *
        * * *
      * * * * *
    * * * * * * *
The one who takes the last item looses.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/


% The state of the game is represented as a list containing the number of items for each row
game :-
	play([1, 3, 5, 7], []).

play(State, History) :-			% 'History' is the list of visited states
	gameOver(State, Result), 
	!,      % someone has won; 'Result' indicates who won
	write('.'),
	analyse(History, Result).	% analyses the ended game as a list of states ('History')

play(State, History) :-
	display(State),
	askMove(State, NState1), % asks for the opponent's move
	display(NState1),    % performs the opponent's move
	move(NState1, NState), % computes the machine's move
	play(NState, [NState1, State|History]).	% stores the two new states before looping


% 'gameOver' returns 1 if there is one item left, and 0 if there is no item left
gameOver([0|L], R) :- gameOver(L, R).	% discarding empty rows
gameOver([1|L], 1) :- gameOver(L, 0).	% one single item left: user looses if other rows are empty
gameOver([], 0).	% machine has lost

analyse(History, Result) :-
	keepInMind(History,Result), % learns from the game
	victory(Result).    % just says who won

victory(_) :-
        silent, !.	% no output when the 'silent' flag is raised
victory(1) :-
	write('J''ai gagne !!'), nl.
victory(0) :-
	write('J''ai perdu !!'), nl.

askMove(State, NState) :-
        silent,		% no output when the 'silent' flag is raised
        !,
        move(State, NState).    
        
askMove(State, NState) :-
	repeat_forever, 	% repeat_forever acts as a bouncing wall for backtracking
	write('A vous : '), nl, 
	write('Nombre de croix : '),
	get(NChar),	% gets non-space ascii character
	Number is NChar - 48, % 48 == ASCII code of '0'
	write('Rangee : '), 
	get(RChar),
	Row is RChar - 48, % 48 == ASCII code of '0'
	modify(State, NState, Row, Number), % implements the move
	!.

move(State, NState) :-
        % computes the best available move
	findall(Act, transition(State, _, Act), ActList),  % list of available moves
	best(ActList, A), 
	transition(State, NState, A), % retrieves the resulting state
	comment(A).

transition(State, NState, act(R, N, V)) :-
	row(State, R, N),   % returns all moves available from 'State' in sequence
	modify(State, NState, R, N), % creates the corresponding new state
	value0(NState, V).  % evaluates the state

comment(_) :-
        silent, !.
comment(act(R, N, V)) :-
	write('J''en prends '), 
	write(N), write(' en '), write(R), 
	write(' (value: '), write(V), write(')'), nl.

modify([L|State], [L1|State], 1, Number) :-
	Number > 0,
	L >= Number, 
	L1 is L - Number, 
	!.
modify([L|State], [L|State1], Row, Number) :-
	Row > 1, 
	Row1 is Row - 1, 
	!, 
	modify(State, State1, Row1, Number).
modify(E, E, _, _) :-
	nl, 
	write('!!! coup impossible !!!'), nl, 
	fail.

%%%%%%%%%%%%%
% memory   %
%%%%%%%%%%%%%

% predicate 'value' is used to memorize the attractiveness of states
clean :-
	retractall(value(_, _)).

% 'keepInMind' takes all intermediary states of an ended game
% and valuates each of them depending on whether the winner or the looser went through it
keepInMind([], _).
keepInMind([H|HR], 0) :-
	memory(H, 1), 
	!, 
	keepInMind(HR, 1).
keepInMind([H|HR], 1) :-
	memory(H, -1), 
	!, 
	keepInMind(HR, 0).

memory(E, Incr) :-
	retract(value(E, V)), 
	!, 
	V1 is V + Incr, 
	assert(value(E, V1)).
memory(E, Incr) :-
	assert(value(E, Incr)).

value0(State, V) :-
	value(State, V), % know state
	!.
value0(_State, 0).  % the state was never encountered before

% 'best' finds the best valued act within a list of acts
best([A|AList], A1) :-
	best1(AList, A, A1).

best1([], B, B).
best1([A|AList], B, A1) :-
	A = act(_, _, V), 
	B = act(_, _, V1), 
	V > V1, 
	!, 
	best1(AList, A, A1).
best1([_|AList], B, A1) :-
	best1(AList, B, A1).

% 'learn' asks the machine to play N silent games with itself
learn(N) :-
	% You may use a combination  repeat(N),.... fail.
	% to have the program play N games against itself
    repeat(N),
    assert(silent),
    play([1,3,5,7],[]),
    retract(silent),	% dummy instruction to avoid warning
	fail.

%%%%%%%%%%%%%
% display   %
%%%%%%%%%%%%%

display(_) :-	
	silent,
	!.
display([N1|R]) :-	
	display(1, [N1|R]).
display(Row, [N1|R]) :-	
	show(Row, N1), 
	Row1 is Row + 1, 
	display(Row1, R).
display(_, []) :- 
	nl, nl.

show(Row, N) :-
	N > 0, 
	N1 is N - 1, 
	show(Row, N1), 
	write('X ').
show(Row, 0) :-
	nl, 
	write(Row), 
	write(' .........  ').

%%%%%%%%%%%%%%%
% utilitaires %
%%%%%%%%%%%%%%%

repeat_forever.
repeat_forever :-
	repeat_forever.
	
repeat(_).
repeat(N) :-
	N > 0, 
	N1 is N - 1, 
	repeat(N1).

% 'row' sorts through the rows, and for each,
% calls 'number' with the actual number of items in the row
row([N|_], 1, Nb) :-
	number(N, Nb).
row([_|L], R, Nb) :-
	row(L, R1, Nb), 
	R is R1 + 1.

% 'number' sorts through all possible values below the total number of items
number(R, 1) :-
	R >0.
number(R, N) :-
	R > 1, 
	R1 is R - 1, 
	number(R1, N1), 
	N is N1 + 1.


/*---------------------------------------------------------------*/
/* Telecom Paris- J-L. Dessalles 2023                            */
/* LOGIC AND KNOWLEDGE REPRESENTATION                            */
/*            http://teaching.dessalles.fr/LKR                   */
/*---------------------------------------------------------------*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% symbolic concept learning %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- select(_,_,_).   % to circumvent a bug in my version of SWI-Prolg

:- dynamic(vPosition/2).

%----------------------%
% Geometrical ontology %
%----------------------%
/*
                form
            /           \
     polygon            ellipsoid
  /  /	 |    \         /       \
sq. tr. pent. hex.    ellipse  circle

*/
parent(form, polygon).
parent(form, ellipsoid).
parent(polygon, square).
parent(polygon, triangle).
parent(polygon, pentagon).
parent(polygon, hexagon).
parent(ellipsoid, ellipse).
parent(ellipsoid, circle).


%------------------------------------------------------------%
% Least general generalization between two geometrical shapes
%------------------------------------------------------------%

lgg(Shape, Shape, Shape, 0).

lgg(Shape1, Shape2, Shape, Cost) :-
    ancestor(Shape, Shape1, Cost1),
    ancestor(Shape, Shape2, Cost2),
    Cost is Cost1 + Cost2.

ancestor(X,X,0).	% no climbing at first
ancestor(X,Y,N) :-
	parent(X,Z), % climbing the hierarchy
	ancestor(Z,Y,N1),
    !,	% recursive call, just in case one needs to climb further up
	N is N1 +1. % there is a cost in climbing up the hierarchy

/*--------------------------------------------------*/
/* Unification between two feature structures       */
/* Calls 'lgg' to unify features that are not equal */
/*--------------------------------------------------*/
match([],[],[],0).
match([Feature1|FS1], FS2, [F|FS], Cost) :-
    % Looks for compatible features between the two feature structures
    select(Feature2, FS2, FS2a),
    compatible(Feature1, Feature2, F, Cost1),
    match(FS1, FS2a, FS, Cost2),
    Cost is Cost1 + Cost2.

compatible(F1, F2, F, Cost) :-
    F1 =.. [Shape1|A],
    F2 =.. [Shape2|A],
    lgg(Shape1, Shape2, Shape, Cost),
    F =.. [Shape|A].
    
%------------------------------------------------------------------%
% interface with "perception":                                     %
% executing objects means that they are given a definite reference %
%------------------------------------------------------------------%
execute([]).
execute([P|Ps]) :-
    P,
    execute(Ps).
execute([P|Ps]) :-
    P =.. [Pr|Var], % P has failed
    parent(PPr,Pr), % climb up the hierarchy to make another attempt
    PP =.. [PPr|Var],
    execute([PP|Ps]).

% object instantiation
form(object_1).	% object is instanciated regardless of its specific properties
form(object_2).	% object is instanciated regardless of its specific properties

% Vertical position
vPos(Object,P) :-
    not(vPosition(Object,_)),
    assert(vPosition(Object,P)).	% objects are given positions only once
vPos(Object,P) :-
    retract(vPosition(Object,P)),    % cleans memory when backtracking
    fail.

%-------%
% Tests %
%-------%
generalize(E1, E2) :-
    write(E1),nl,
    write(E2),nl,
    match(E1, E2, E, Cost),
    execute(E),
    write(E), write(' --- '), write(Cost), nl.

matchTest :-
    E1 = [square(A), circle(B), vPos(A,2), vPos(B,1)],  
    E2 = [triangle(C), square(D), vPos(C,2), vPos(D,1)],
    generalize(E1,E2).

/*---------------------------------------------------------------*/
/* Telecom Paris- J-L. Dessalles 2023                            */
/* LOGIC AND KNOWLEDGE REPRESENTATION                            */
/*            http://teaching.dessalles.fr/LKR                   */
/*---------------------------------------------------------------*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% explanation_based generalization %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



/*-----------------------*/
/*  Background knowledge */
/*-----------------------*/
telephone(T) :- connected(T), partOf(T, D), dialingDevice(D), emitsSound(T).
connected(X) :- hasWire(X, W), attached(W, wall).
connected(X) :- feature(X, bluetooth).
connected(X) :- feature(X, wifi).
connected(X) :- partOf(X, A), antenna(A), hasProtocol(X, gsm).
dialingDevice(DD) :- rotaryDial(DD). 
dialingDevice(DD) :- frequencyDial(DD).
dialingDevice(DD) :- touchScreen(DD), hasSoftware(DD, DS), dialingSoftware(DS).
emitsSound(P) :-    hasHP(P).
emitsSound(P) :-    feature(P, bluetooth).

/*---------*/
/* Example */
/*---------*/
example(myphone, Features) :-
    Features = [silver(myphone), belongs(myphone, jld), partOf(myphone, tc),
                touchScreen(tc), partOf(myphone, a), antenna(a),
                hasSoftware(tc, s1), game(s1),
                hasSoftware(tc, s2), dialingSoftware(s2),
                feature(myphone,wifi), feature(myphone,bluetooth),
                hasProtocol(myphone, gsm), beautiful(myphone)].


/*-------------------*/
/* Prover without trace */
/*-------------------*/
prove((G,Q)) :-		% forms like (H,T) are returned by 'clause' (see below)
    !,
    prove(G),
    prove(Q).
prove(true) :-
    !.
prove(G) :-
    known(G),
    write('\t'), write(G), write(' is known'), nl.
prove(G) :-
    write('attempting to prove '),write(G),nl,
    clause(G,Q),	% Q may have the following form: (H,T) (where T may be also like Q)
    prove(Q),
    write(G), write(' has been proven'), nl.


/*------*/
/* Test */
/*------*/
ebgTest :-
    retractall(known(_)),
    example(M, F),
    assertL(F),
    prove(telephone(M)).

assertL([F|Fs]) :-
    assert(known(F)),
    assertL(Fs).
assertL([]).
