/* Definite clause Grammars Code */  
s --> np, vp.
np --> det, noun.
vp --> verb, np, pp.
pp --> prep,np.
prep -->[to].
det --> [the].
verb --> [brought].
noun --> [waiter].
noun --> [meal].
noun --> [table].
