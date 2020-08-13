s(s(NP,VP))  --> np(NP), vp(VP).
np(np(D,N))  --> det(D), noun(N).
vp(vp(V,NP,PP)) --> verb(V), np(NP), pp(PP).
pp(pp(P,NP)) --> prep(P), np(NP).
det(det(the)) --> [the].
prep((prep)) --> [to].
verb(verb(brought)) --> [brought].
noun(noun(waiter)) --> [waiter].
noun(noun(meal)) --> [meal].
noun(noun(table)) --> [table].
