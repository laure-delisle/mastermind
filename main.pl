% - tirage(N,L) -%
% vérifie que L soit une liste de quatre nombres
% tirés aléatoirement dans l’intervalle [0, N[
tirage(N,[A,B,C,D]) :-
	random(0,N,A),
	random(0,N,B),
	random(0,N,C),
	random(0,N,D).

% - bien_place(X,L1,L2) -%
% réussit si le nombre X est à la même place dans
% la liste L1 et dans la liste L2
bien_place(X,[X,_,_,_],[X,_,_,_]).
bien_place(X,[_,X,_,_],[_,X,_,_]).
bien_place(X,[_,_,X,_],[_,_,X,_]).
bien_place(X,[_,_,_,X],[_,_,_,X]).

% - nb_corrects(L1,L2,N) -%
% indique pour N le nombre de pions placés au même
% endroit dans les listes L1 et L2
nb_corrects(L1,L2,N) :-
	findall(X,bien_place(X,L1,L2),L),
	length(L,N).

% - nb_communs(L1,L2,N) -%
% calcule le nombre de pions en commun entre deux
% combinaisons
nb_communs([],_,0).
nb_communs([X|T],L,N) :-
	select(X,L,R), !,
	nb_communs(T,R,M),
	N is (M+1).
nb_communs([_|T],L,M) :-
	nb_communs(T,L,M).

% - resultat(L1,L2,B,M) -%
% détermine que, si l’on compare les listes L1 et L2,
% il y a B pions bien placés et M pions mal placés
resultat(L1,L2,B,M) :-
	nb_corrects(L1,L2,B),
	nb_communs(L1,L2,N),
	M is N-B.

% - solution(S) -%
% indique que S est la solution
% -- /!\ prédicat dynamique non déclaré dans le .pl

% - coup(P,B,M) -%
% garde en mémoire les coups joués, avec P combinaison
% tentée, B nombre de pions bien placés, M nombre de
% pions mal placés
% -- /!\ prédicat dynamique non déclaré dans le .pl

% - partie(N) -%
% réinitialise la partie, choisit une combinaison à
% deviner, avec N nombre de couleurs permises
partie(N) :-
	randomize,
	retractall(solution(_)),
	retractall(coup(_,_,_)),
	tirage(N,S),
	asserta(solution(S)).

% - propose(P) -%
% permet au joueur de proposer une combinaison P,
% détermine les bien et mal placés, stocke l’information
% que le coup a été joué
propose(P) :-
	solution(S),
	resultat(P,S,B,M),
	write('bien place : '),write(B),
	write(' mal place : '),write(M),
	asserta(coup(P,B,M)),
	B=4,
	findall(X,coup(X,_,_),L),
	length(L,NC),
	write(' -> Vous gagnez en '), write(NC),
	write(' coups.').


% -------- Faisons TRICHER prolog -------- %

% - compatible(S,L) -%
% vérifie que S est compatible avec tous les coups de la
% liste L (i.e. qu’en confrontant S à P on obtient B bien
% placés et M mal placés)
% avec S une combinaison, L une liste de listes de la forme [P,B,M]
% représentant un coup (P la combinaison, B le nombre de bien
% placés, M le nombre de mal placés)
% -- /!\ fait TRICHER prolog
compatible(_,[]).
compatible(S,[H|T]) :-
	H=[P,B,M],
	resultat(P,S,B,M),
	compatible(S,T).

% - genere_solution(L,LC,S) -%
% crée une nouvelle solution pour tricher, qui soit
% compatible avec tous les coups déjà joués
% avec L la nouvelle solution, LC la liste des coups,
% S l'ancienne solution
% -- /!\ fait TRICHER prolog
genere_solution([A,B,C,D],LC,S) :-
	nbcouleur(N),
	% introduction de "randomness" dans l'incrementation
	for(B,0,N), for(C,0,N),
	for(A,0,N), for(D,0,N),
	[A,B,C,D] \= S,
	compatible([A,B,C,D],LC),!.

% - change_solution(P,S1,LC) -%
% si le joueur a trouvé la solution, remplace cette
% ancienne solution par une nouvelle générée
% avec P le coup joué, S1 la solution à l'issue du
% traitement, LC la liste de coups joués précédemment
% -- /!\ fait TRICHER prolog
change_solution(P,S1,LC) :-
	solution(S),
	resultat(P,S,B,M),
	M=0,
	B=4,
	genere_solution(S1,LC,S),
	retractall(solution(_)),
	asserta(solution(S1)),!.
change_solution(_,S,_) :-
	solution(S).

% - partie_bis(N) -%
% réinitialise la partie, choisit une combinaison à
% deviner
% avec N nombre de couleurs permises
% -- /!\ fait TRICHER prolog
partie_bis(N) :-
	randomize,
	retractall(nbcouleur(_)),
	retractall(solution(_)),
	retractall(coup(_,_,_)),
	tirage(N,S),
	asserta(solution(S)),
	asserta(coup(0)),
	asserta(nbcouleur(N)).

% - propose_bis(P) -%
% permet au joueur de proposer une combinaison P,
% détermine les bien et mal placés, stocke l’information
% que le coup a été joué
% avec P la combinaison proposée
% -- /!\ fait TRICHER prolog
propose_bis(P) :-
	findall([PC,BC,MC],coup(PC,BC,MC),L),
	change_solution(P,S,L),
	resultat(P,S,B1,M1),
	asserta(coup(P,B1,M1)),
	write('bien place : '),write(B1),
	write(' mal place : '),write(M1),
	B1=4,
	length(L,NC),
	write(' -> Vous gagnez en '), write(NC),
	write(' coups.').






