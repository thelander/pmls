:- module(distributions, [ranf/1, normal/3, beta/2, gamma/3]).
:- use_module([library(random)]).

%%
% Different distributions that are generated according to parameters 

%%
% random floats 0 <= x < 1
%
ranf(X):-     
	N =  65536,
	% X is random(N) returns a 
        % random number from 0 .. N-1
        random(0, N, Rand),
	X is Rand/N.

%%
% Using ranf to generate a normal distribution
% -inf <= x <= inf, mean=M, standard deviation=S
normal(M, S, N):-
	box_muller(M, S, N).

% classical fast method for computing
% normal functions using polar co-ords
box_muller(M, S, N):-
	w(W0, X),
	W is sqrt((-2.0 * log(W0)) / W0),
	Y1 is X * W,
	N is M + Y1 * S.

w(W, X):-
	ranf(RX1),
	X1 is 2.0 * RX1 - 1,
	ranf(RX2),
	X2 is 2.0 * RX2 - 1,
	W0 is X1*X1 + X2*X2,
	(W0  >= 1.0 ->
	  w(W,X)
	;
	  W0=W, X = X1
	).

%%%
% Beta distribution
% simple beta distributions (0 <= x <=1, mean=B)
% only B = 0.1, 0.2, 0.3, ... 0.9, 1 is supported
beta(B, X):-
	beta1(B,X),!.
beta(B, X):-
	B1 is 1 - B,
	beta1(B1,Y),
	X is 1 - Y.

beta1(0.50, Y):-
	ranf(X),	
	Y is X.
beta1(0.60, Y):-
	ranf(X),	  	
	Y is X^0.67.
beta1(0.67, Y):-
	ranf(X),	
	Y is X^0.5.
beta1(0.75, Y):-
	ranf(X),	
	Y is X^0.33.
beta1(0.80, Y):-
	ranf(X),	
	Y is X^0.25.
beta1(0.90, Y):-
	ranf(X),	
	Y is X^(1/9).
beta1(1, 1).

%%%
% Gamma distribution 
% 0 <= x <= inf
% mean = Mean, skew= Alpha
% at low skew, (e.g. x=2), x always near mean
% at skew=20, gamma becomes normal

% only supports integer values X and Alpha
gamma(Mean, Alpha, Out) :-
	Beta is Mean / Alpha,
	(Alpha > 20 ->
	  Mean is Alpha * Beta,
	  Sd is sqrt(Alpha * Beta * Beta),
          normal(Mean, Sd, TOut),
	  Out = TOut
	;
	  gamma(Alpha, Beta, 0, Out)
	).

gamma(0, _, X, X) :- !.
gamma(Alpha, Beta, In, Gamma) :-
	ranf(X),
	Temp is In + ( -1 * Beta * log(1-X)),
	Alpha1 is Alpha - 1,
	gamma(Alpha1, Beta, Temp, Gamma).
