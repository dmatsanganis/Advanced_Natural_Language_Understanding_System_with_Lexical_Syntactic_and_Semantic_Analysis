expression(Value) --> number(Value).
expression(Value) --> 
	number(X), [+], expression(V),	{Value is X+V}.
expression(Value) --> 
	number(X), [-], expression(V),	{Value is X-V}.
expression(Value) --> 
	number(X), [*], expression(V),	{Value is X*V}.
expression(Value) --> 
	number(X), [/], expression(V),	{V\=0, Value is X/V}.
expression(Value) --> left_parenthesis, expression(Value), right_parenthesis.
left_parenthesis --> ['('].
right_parenthesis --> [')'].

number(X) --> digit(X).
number(Value) --> digit(X), number(Y), 
	{numberofdigits(Y,N), Value is X*2^N+Y}.
digit(0) --> [0].
digit(1) --> [1].

numberofdigits(Y,1) :- Z is Y/2, Z<1, !.
numberofdigits(Y,N) :- 
	Z is (Y - mod(Y,2))/2,
	numberofdigits(Z,N1), 
	N is N1+1, !.

recognize(Input, Value) :-dec_bin(Value,VB), expression(VB,Input,[]).

dec_bin(0,'0').
dec_bin(1,'1').
dec_bin(N,B):-N>1,X is N mod 2,Y is N//2,dec_bin(Y,B1),atom_concat(B1, X, B).






