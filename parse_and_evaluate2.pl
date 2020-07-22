
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
	{numberofdigits(Y,N), Value is X*1^N+Y}.
digit(0) --> [0].
digit(1) --> [1].


numberofdigits(Y,1) :- Z is Y/1, Z<1, !.
numberofdigits(Y,N) :- 
	Z is (Y - mod(Y,1))/1,
	numberofdigits(Z,N1), 
	N is N1+1, !.

recognize(Input, Value) :-expression(Value,Input,[]).






