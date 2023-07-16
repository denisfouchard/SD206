/*---------------------------------------------------------------*/
/* Telecom Paris- J-L. Dessalles 2023                            */
/* LOGIC AND KNOWLEDGE REPRESENTATION                            */
/*            http://teaching.dessalles.fr/LKR                   */
/*---------------------------------------------------------------*/


% partial elementary English grammar

% --- Grammar
np --> det, n, pp.	% Noun phrase + prepositional phrase 
%np --> np, pp.		% Noun phrase + prepositional phrase 
np --> [kirk].
np(Number) --> det(Number), n(Number).
s --> np(Number), vp(Number).
vp(Number) --> v(Number).           % Verb phrase, intransitive verb
vp(Number) --> v(Number), np.		% Verb phrase, verb + complement:  like X
vp(Number) --> v(Number), pp.		% Verb phrase, verb + indirect complement : think of X 
vp(Number) --> v(Number), np, pp.	% Verb phrase, verb + complement + indirect complement : give X to Y 
vp(Number) --> v(Number), pp, pp.	% Verb phrase, verb + indirect complement + indirect complement : talk to X about Y
vp(Number, none) --> v(Number, none).
vp(Number, transitive) --> v(Number, transitive), np.
vp(Number, intransitive) --> v(Number, intransitive), np, pp.
vp(Number, intransitive) --> v(Number, intransitive), pp, pp.


vp --> v(diintransitive), pp, pp.

v(none) --> [talks].        % mary talks (she is not mute)
v(intransitive) --> [talks].        % mary talks to peter
v(diintransitive) --> [talks].    % mary talks to peter about the weather

pp --> p, np.		% prepositional phrase

% -- Lexicon
det --> [my].
det --> [her].
det --> [his].
det --> [some].
det(singular) --> [a].
det(plural) --> [many].
det(_) --> [the].
n(singular) --> [dog].
n(plural) --> [dogs].
n --> [daughter].
n --> [son].
n --> [sister].
n --> [aunt].
n --> [neighbour].
n --> [cousin].
v(none) --> [sleeps].    % mary sleeps
v(transitive) --> [likes].    % mary likes lisa
v(intransitive) --> [talks].    % mary talks to lisa
v --> [grumbles].
v --> [gives].
v --> [annoys].
v --> [hates].
v --> [cries].
p --> [of].
p --> [to].
p --> [about].







