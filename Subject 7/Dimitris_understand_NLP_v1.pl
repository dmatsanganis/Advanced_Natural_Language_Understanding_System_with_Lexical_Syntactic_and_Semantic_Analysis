/*----------------------------------------------------------------------*/
/* 		PROJECT 	ON			*/
/* 	NATUTAL LANGUAGE PROCESSING (NLP)	*/
/*----------------------------------------------------------------------*/
/* Author :Themis Panayiotopoulos & Dimitrios Matsanganis			*/
/* 							  June 2020					*/
/*----------------------------------------------------------------------*/
/* 'Natural Language Understanding : 			*/
/*                              from a story to a knowledge base'	*/
/*----------------------------------------------------------------------*/
/*      1. Lexical Analysis				*/
/*      2. Syntactic analysis				*/
/*      3. Semantic analysis				*/
/*      4. Updating the Knowledge Base			*/
/*      5. Inserting Info to knowledge base	using tell(Sentence)	*/
/*      6. asking questions about the story using ask(Question)	*/
/*----------------------------------------------------------------------*/
/* Top query 					*/
/*	?- understand(‘cat_story.txt’),			*/
/*	?- understand(‘victoria_story.txt’),			*/
/*----------------------------------------------------------------------*/
/* story in cat_story.txt :					*/
/* the cat needs food. the cat has the food. the cat hates the waterbath. 	*/
/* the cat runs fast. the cat jumps the fence. the cat is cute.		*/
/*----------------------------------------------------------------------*/
/* story in victoria_story.txt :				*/
/* victoria is a girl. victoria is tall. victoria is slim. victoria is blonde.*/
/* victoria gives james a cat. victoria gives katerina a candy.	*/
/* victoria loves icecreams. victoria loves anime.			*/
/*----------------------------------------------------------------------*/
:- discontiguous ask/1, q/5.
/*----------------------------------------------------------------------*/
/* Vocabulary elements 		*/
/*----------------------------------------------------------------------*/
:- dynamic chased/2, hates/2, has/2, needs/2, scary/1.

/*---------------------------------------------------------------------*/
/* Verbs (v) 					*/
/*---------------------------------------------------------------------*/
:- dynamic loves/2, love/2, hate/2, have/2, kicks/2, jumps/2.

/*---------------------------------------------------------------------*/
/* Auxiliary Verbs (av) 				*/
/*---------------------------------------------------------------------*/
:- dynamic does/2, are/2, do/2.

/*---------------------------------------------------------------------*/
/* Intransitive Verbs (iv) 				*/
/*---------------------------------------------------------------------*/
:- dynamic runs/1,hurts/1,walks/1,jumps/1,shoots/1.
:- dynamic runs/2,hurts/2,walks/2.

/*---------------------------------------------------------------------*/
/* Transitive Verbs (tv) 				*/
/*---------------------------------------------------------------------*/
:- dynamic gives/2, gave/2.

/*---------------------------------------------------------------------*/
/* Adjectives (adj) 					*/
/*---------------------------------------------------------------------*/
:- dynamic tall/1,short/1,blonde/1,slim/1,fat/1.

% Success Messages.
understand(Story) :- 
lexical_analyse(Story, Sentences), write('the lexical analysis completed!'), nl,nl,nl,
syntax_analyse(Sentences, Syntax), write('the syntactic analysis completed!'),nl,nl,nl,
semantics_analyse(Sentences, Semantics), write('the semantic analysis completed!'), nl,nl,nl,
update_knowledge_base(Semantics), nl,write('Knowledge Base Updated!'),nl,nl,nl.


/*==========================================================*/
/* 		LEXICAL ANALYSIS			*/
/*==========================================================*/

/*----------------------------------------------------------------------*/
/* Reads a text from a file and produces a list of sentences. 	*/
/* Each sentence is a list of words			*/
/*----------------------------------------------------------------------*/

% First Read from File then analyse.
lexical_analyse(Text, Result) :- 
	% Text = 'victoria_story.txt'
	see(Text),         		 /* open this file to read */
	read(X),			/* read from File */ 
	seen,			/* close the input File */ 
	analyse(X,Result),		/* Lexical Analysis */
	write_results(Result),	/* Wite Result */
	!.			/* stop now */ 

/*----------------------------------------------------------------------*/
/* analyse 					*/
/* 1. turn imput into list of ascii codes			*/
/* 2. group together ascii codes belonging to same word	*/
/*----------------------------------------------------------------------*/
% If we reach End of File then stop
analyse(end_of_file,[]) :- !. 

% analyse 
analyse(Input,All) :- input_to_sentences(Input,All).

/*----------------------------------------------------------------------*/
/* Input to ascii 					*/
/* turn input into list of ASCII codes, pass list to tokeniser 	*/
/*----------------------------------------------------------------------*/
input_to_sentences(Input, List_of_Words):- 
	name(Input,Ascii_List), 
	tokenise(Ascii_List, List_of_Words).

/*----------------------------------------------------------------------*/
/*	TOKENISER				*/
/*----------------------------------------------------------------------*/

% If no ASCII codes left then stop
tokenise([],[]):-!.

% identify first sentence, move onto rest of sentences
tokenise(Ascii_List,All):-
	one_sentence(Ascii_List, Sentence, Rest, T),
	T=end_of_sentence,
	tokenise(Rest, List_of_Sentences),
	append([Sentence],List_of_Sentences,All).

/*----------------------------------------------------------------------*/
/*	ONE SENTENCE				*/
/* one_sentence(Ascii_List, Sentence, Rest, end_of_sentence)  */
/* From List of Ascii codes identify a sentence		*/
/*----------------------------------------------------------------------*/

% full-stop = end of sentence
one_sentence(Ascii_List, [Word], Rest_Ascii, end_of_sentence):-
   one_word(middle_of_word, Ascii_List, Ascii_Word, Rest_Ascii, T),
   T=end_of_sentence,
   name(Word,Ascii_Word), !.

% end of text = no codes left
one_sentence([], [], [], end_of_text):- !. 

/*----------------------------------------------------------------------*/
/* if not the end of a sentence then add code to output list and 	*/
/* call recursively					*/
/*----------------------------------------------------------------------*/
one_sentence(Ascii_List, Sentence, Rest_Text, Type):-
   one_word(start_of_word, Ascii_List, Ascii_Word, Rest_Ascii, T),
   T=end_of_word,
   name(Word,Ascii_Word),
   one_sentence(Rest_Ascii, Rest_Words, Rest_Text, Type),
   append([Word], Rest_Words, Sentence).


/*---------------------------------------------------------------------*/
/* End of ONE SENTENCE				*/
/*---------------------------------------------------------------------*/

/*----------------------------------------------------------------------*/
/*	ONE WORD				*/
/*----------------------------------------------------------------------*/
% Terminate recursion :
% end of text = no codes left
one_word(middle_of_word, [], [], [], end_of_text):-  !.
% full-stop = end of sentence
one_word(middle_of_word, [46|T], [], T, end_of_sentence):-  !. 
% space = end of word
one_word(middle_of_word, [32|T], [], T, end_of_word):-  !. 

/*----------------------------------------------------------------------*/
/* if not the end of a word then add code to output list and 	*/
/* recurse						*/
/*----------------------------------------------------------------------*/
% ignore Carriage return (Ascii 13)
one_word(Any, [13|T], Word, Rest_Codes, Type):- 
	one_word(Any, T,  Word,  Rest_Codes, Type).
% ignore Line feed (Ascii 10)
one_word(Any, [10|T], Word, Rest_Codes, Type):- 
	one_word(Any, T,  Word,  Rest_Codes, Type).
% ignore leading space
one_word(start_of_word, [32|T], Word, Rest_Codes, Type):- 
	one_word(start_of_word, T,  Word,  Rest_Codes, Type).
% We have moves to analysing the word
one_word(_, [Ascii_Code|T], [Ascii_Code|Word], Rest_Codes, Type):-
	Ascii_Code \= 32,
	one_word(middle_of_word, T, Word, Rest_Codes, Type).
/*---------------------------------------------------------------------*/
/* 						End of ONE WORD				*/
/*---------------------------------------------------------------------*/


/*==========================================================*/
/* 						END OF LEXICAL ANALYSIS			*/
/*==========================================================*/

/*==========================================================*/
/* 						SYNTACTIC ANALYSIS		*/
/*==========================================================*/

/*----------------------------------------------------------------------*/
/* Takes as input a list of sentences and produces their	*/
/* syntax trees					*/
/*----------------------------------------------------------------------*/
syntax_analyse(Sentences, Structures) :- 
   syntactic_analysis(Sentences, Structures),	% Syntactic Analysis
   write_results(Structures),			% Wite Result 
   !.					% stop now  

syntactic_analysis([],[]) :- !. 
syntactic_analysis([Sentence|Sentences], [Structure|Structures]) :- 
   snt(Structure, Sentence, []),
   syntactic_analysis(Sentences, Structures), !.

/*==========================================================*/
/* 							GRAMMAR RULES			*/
/*==========================================================*/

/*---------------------------------------------------------------------*/
/* The s rules of Syntactic Analysis			*/
/*---------------------------------------------------------------------*/
/* To test:					*/
/*  ?- snt(Structure,[the,cat,jumps,the,fence],[]).		*/
/* Structure = 					*/
/*      s(np(d(the),n(cat)),vp(v(jumps),np(det(the),n(fence)))) 	*/
/*---------------------------------------------------------------------*/

/*---------------------------------------------------------------------*/
/* Sentence (snt) 					*/
/* Proper Nouns (pn) 				*/
/* Intransitive Verbs (iv) 				*/
/* Auxiliary Verbs (av) 				*/
/* Verbs (v) 					*/
/* Transitive Verbs (tv) 				*/
/* Adverb (adv) 					*/
/* Adjectives (adj) 					*/
/* Determiner (det) 					*/
/* Noun (n) 					*/
/* Noun Phrase (np) 					*/
/* Verb Phrase (vb) 					*/
/*---------------------------------------------------------------------*/

/*---------------------------------------------------------------------*/
/* Sentence (snt) 					*/
/*---------------------------------------------------------------------*/
snt(s(NP,VP)) 	--> np(NP), vp(VP).

/*---------------------------------------------------------------------*/
/* Noun Phrase (np) 					*/
/*---------------------------------------------------------------------*/
np(np(N))		--> pn(N).
np(np(D,N))	--> det(D), n(N).
np(np(N))		--> n(N).

/*---------------------------------------------------------------------*/
/* Verb Phrase (vb) 					*/
/*---------------------------------------------------------------------*/
% Intransitive verbs :
vp(vp(V)) 		--> iv(V). 
vp(vp(V,ADV)) 	--> iv(V), adv(ADV). 
% Auxiliary verbs
vp(vp(AV,A)) 	--> av(AV), adj(A).
% Transitive verbs :
vp(vp(TV, PN, NP)) 	--> tv(TV), np(PN), np(NP).
% verbs
vp(vp(V,NP)) 	--> v(V), np(NP).

/* ============================================================================== */
/* 							VOCABULARY OF EXAMPLE			*/
/*===============================================================================*/
/* 						  THE cat_story.txt EXAMPLE */
/*===============================================================================*/
/* the cat needs food. the cat has the food. the cat hates the waterbath. 	*/
/* the cat runs fast. the cat jumps the fence. the cat is cute.	*/
/*---------------------------------------------------------------------*/
/* det : the	 */
/* verbs : needs, has, hates, jumps, is	*/
/* adjectives : cute, fast			*/
/* nouns : cat, dog, food, waterbath, fence	*/
/*===============================================================================*/
/* 						 THE victoria_story.txt EXAMPLE */
/*===============================================================================*/
/* victoria is a girl. victoria is tall. victoria is slim. victoria is blonde. 	*/
/* victoria gives james a cat. victoria gives katerina a candy. */
/* victoria loves icecreams. victoria loves anime.	*/
/*---------------------------------------------------------------------*/
/* det : the, a	*/
/* verbs : gives, loves, is	*/
/* adjectives : tall, slim, blonde			*/
/* nouns : cat, girl, candy, icectreams, anime	*/
/* proper nouns : victoria, james, katerina	*/
/*---------------------------------------------------------------------*/

/*---------------------------------------------------------------------*/
/* 							Intransitive Verbs (iv) 				*/
/*---------------------------------------------------------------------*/
% needed for example :
iv(iv(runs))-->[runs].
iv(iv(jumps))-->[jumps].

% extension of vocabulary :
iv(iv(run))-->[run].
iv(iv(running))-->[running].
iv(iv(hurts))-->[hurts].
iv(iv(hurt))-->[hurt].
iv(iv(hurting))-->[hurting].
iv(iv(walks))-->[walks].
iv(iv(walk))-->[walk].
iv(iv(walking))-->[walking].
iv(iv(jump))-->[jump].
iv(iv(jumping))-->[jumping].
iv(iv(shoots))-->[shoots].
iv(iv(shoot))-->[shoot].
iv(iv(shooting))-->[shooting].

/*---------------------------------------------------------------------*/
/* Auxiliary Verbs (av) 				*/
/*---------------------------------------------------------------------*/
% needed for example :
av(av(is))-->[is].

% extension of vocabulary :
av(av(does))-->[does].
av(av(are))-->[are].
av(av(do))-->[do].

/*---------------------------------------------------------------------*/
/* Transitive Verbs (tv) 				*/
/*---------------------------------------------------------------------*/
% needed for example :
tv(tv(gives))	-->[gives].

% extension of vocabulary : 
tv(tv(give))	-->[give]. 
tv(tv(gave))	-->[gave]. 
tv(tv(giving))	-->[giving].

/*---------------------------------------------------------------------*/
/* Verbs (v) 					*/
/*---------------------------------------------------------------------*/
% needed for example :
v(v(jumps))-->[jumps]. 
v(v(needs))-->[needs].
v(v(need))-->[need].
v(v(hates))-->[hates]. 
v(v(hate))-->[hate].
v(v(has))  -->[has].   
v(v(have))  -->[have].
v(v(loves))-->[loves]. 

% extension of vocabulary :
v(v(love))-->[love].
v(v(kicks))-->[kicks]. 
v(v(kick))-->[kick].
v(v(jump))-->[jump].
v(v(chased))-->[chased].
v(v(chase))-->[chase].

/*---------------------------------------------------------------------*/
/* Adjectives (adj) 					*/
/*---------------------------------------------------------------------*/
% needed for example :
adj(adj(cute))-->[cute].

% extension of vocabulary :
adj(adj(tall))-->[tall].
adj(adj(short))-->[short].
adj(adj(blonde))-->[blonde].
adj(adj(slim))-->[slim].
adj(adj(fat))-->[fat].
adj(adj(scary))-->[scary].

/*---------------------------------------------------------------------*/
/* Adverb (adv) 					*/
/*---------------------------------------------------------------------*/
% needed for example :

% extension of vocabulary :
adv(adv(quickly))-->[quickly].
adv(adv(slowly))-->[slowly].
adv(adv(independently))-->[independently].

/*---------------------------------------------------------------------*/
/* Noun (n) 					*/
/*---------------------------------------------------------------------*/
% needed for example
n(n(food))-->[food].
n(n(cat))-->[cat].
n(n(dog))-->[dog].
n(n(book))-->[book].
n(n(icecreams))-->[icecreams].
n(n(candy))-->[candy].
n(n(anime))-->[anime].
n(n(girl))-->[girl].
n(n(fence))-->[fence].

% extension of vocabulary
n(n(dogs))-->[dogs].
n(n(books))-->[books].
n(n(feather))-->[feather].
n(n(feathers))-->[feathers].
n(n(baby))-->[baby].
n(n(babies))-->[babies].
n(n(boy))-->[boy].
n(n(boys))-->[boys].
n(n(girls))-->[girls].
n(n(icecream))-->[icecream].
n(n(cats))-->[cats].


/*---------------------------------------------------------------------*/
/* Proper Nouns (pn) 				*/
/*---------------------------------------------------------------------*/
% needed for example
pn(pn(victoria))-->[victoria].
pn(pn(james))-->[james].
pn(pn(katerina))-->[katerina].

% extension of vocabulary
pn(pn(mary))-->[mary].
pn(pn(john))-->[john].
pn(pn(tomy))-->[tomy].

/*---------------------------------------------------------------------*/
/* Determiner (det) 					*/
/*---------------------------------------------------------------------*/
% needed for example :
det(det(the)) -->[the].
det(det(a))   -->[a].

% extension of vocabulary
det(det(an))   -->[an].

/*==========================================================*/
/* 					END OF SYNTACTIC ANALYSIS		*/
/*==========================================================*/


/*==========================================================*/
/* 						SEMANTIC ANALYSIS		*/
/*==========================================================*/
/*----------------------------------------------------------------------*/
/* Takes as input a list of sentences and produces their	*/
/* semantics - easier if done along with syntactic analysis	*/
/*----------------------------------------------------------------------*/
semantics_analyse(Sentences, AllSemantics) :- 
   semantics_analysis(Sentences, AllSemantics),	% Syntactic Analysis
   write_results(AllSemantics),			% Wite Result 
   !.					% stop now  

semantics_analysis([],[]) :- !. 
semantics_analysis([Sentence|Sentences], [Sem|Semantics]) :- 
   sem(_, Sem, Sentence, []),
   semantics_analysis(Sentences, Semantics), !.

/*==========================================================*/
/* 					SEMANTICS  CREATION  RULES 		*/
/*==========================================================*/

sem(1,Sem) --> sem_np(N), sem_vp(1,V,N1),  	{Sem=..[V,N,N1]}.
% example : [the,cat,jumps,the,fence]
% Sem = jumps(cat,fence)
% example : [victoria,loves,icecreams]
% Sem = loves(victoria,icecreams)
	
sem(2,Sem) --> sem_np(N), sem_vp(2,_,A),  	{Sem=..[A,N]}.
% example : [the,cat,is,cute]
% Sem = cute(cat)
% example : [victoria,is,slim]
% Sem = slim(victoria)

sem(3,Sem) --> sem_np(N), sem_iv(V,s),      {Sem=..[V,N]}.
% example : [cat,runs]
% Sem = runs(cat)
% example : [the,footballer,shoots]
% Sem = shoots(footballer)

sem(4,Sem) --> sem_np(N), sem_iv(V,s), sem_adv(A),	{Sem=..[V,N,A]}.
% example : [cat,runs,quickly]
% Sem = runs(cat,quickly)

sem(5,Sem) -->sem_np(N), sem_tv(V,s), sem_np(N1), sem_np(N2), {Sem=..[V,N,N1,N2]}.
% example : [victoria,gives,katerina,a,candy]
% Sem = gives(victoria,katerina,candy)

/* noun phrase */
sem_np(N)	--> sem_pn(N).
sem_np(N) 	--> sem_det(_), sem_n(N).
sem_np(N) 	--> sem_n(N).

/* verb phrase */
sem_vp(1,V,N) --> sem_v(V,s), sem_np(N).
sem_vp(2,is,A) --> sem_av(is), sem_adj(A).


/*==========================================================*/
/* 					SEMANTICS  VOCABULARY			*/
/*==========================================================*/

/*---------------------------------------------------------------------*/
/* 					Intransitive Verbs (sem_iv) 				*/
/*---------------------------------------------------------------------*/
% needed for example :
sem_iv(runs,s)	-->[runs].
sem_iv(jumps,s)	-->[jumps].


% extension of vocabulary :
sem_iv(runs,q)	-->[running].
sem_iv(hurts,s)	-->[hurts].
sem_iv(hurts,q)	-->[hurting].
sem_iv(walks,s)	-->[walks].
sem_iv(walks,q)	-->[walking].
sem_iv(jumps,q)	-->[jumping].
sem_iv(shoots,s) -->[shoots].
sem_iv(shoots,q) -->[shooting].

/*---------------------------------------------------------------------*/
/* 					Auxiliary Verbs (sem_av) 				*/
/*---------------------------------------------------------------------*/
% needed for example :
sem_av(is)		-->[is].

% extension of vocabulary :
sem_av(does)	-->[does].
sem_av(do)	-->[do].
sem_av(does)	-->[did].
sem_av(are)	-->[are].

/*---------------------------------------------------------------------*/
/* 					Transitive Verbs (sem_tv) 				*/
/*---------------------------------------------------------------------*/
% needed for example :
sem_tv(gives,s)	-->[gives]. 

% extension of vocabulary :
sem_tv(gives,q)	-->[give]. 
sem_tv(gave,s)	-->[gave]. 
sem_tv(giving,q2)	-->[giving].

/*---------------------------------------------------------------------*/
/* 						  Verbs (sem_v) 					*/
/*---------------------------------------------------------------------*/
% needed for example :
sem_v(needs,s)	-->[needs].
sem_v(need,q)	-->[need].
sem_v(hates,s)	-->[hates]. 
sem_v(hate,q)	-->[hate].
sem_v(has,s)  	-->[has].   
sem_v(loves,s)	-->[loves]. 
sem_v(jumps,s)	-->[jumps]. 
sem_v(hates,s)	-->[hates]. 
sem_v(loves,q)	-->[love].

% extension of vocabulary :
sem_v(hates,q)	-->[hate].
sem_v(has,s)  	-->[has].   
sem_v(has,q)  	-->[have].
sem_v(kicks,s)	-->[kicks]. 
sem_v(kicks,q)	-->[kick].
sem_v(jumps,q)	-->[jump].
sem_v(chased,_)	-->[chased].
sem_v(chase,_)	-->[chase].
sem_v(have,q) 	-->[have].


/*---------------------------------------------------------------------*/
/* 					 Adjectives (sem_adj) 				*/
/*---------------------------------------------------------------------*/
% needed for example :
sem_adj(tall)		-->[tall].
sem_adj(blonde)		-->[blonde].
sem_adj(slim)		-->[slim].
sem_adj(slim)		-->[slim].

% extension of vocabulary :
sem_adj(short)		-->[short].
sem_adj(fat)		-->[fat].
sem_adj(scary)		-->[scary].


/*---------------------------------------------------------------------*/
/* 						Adverb (sem_adv) 				*/
/*---------------------------------------------------------------------*/
% needed for example :

% extension of vocabulary :
sem_adv(quickly)		-->[quickly].
sem_adv(slowly)		-->[slowly].
sem_adv(independently)	-->[independently].

/*---------------------------------------------------------------------*/
/* 						 Noun (sem_n) 					*/
/*---------------------------------------------------------------------*/
% needed for example
sem_n(food)		-->[food].
sem_n(cat)		-->[cat].
sem_n(dog)		-->[dog].
sem_n(girl)		-->[girl].
sem_n(fence)	-->[fence].
sem_n(candy)	-->[candy].
sem_n(anime)	-->[anime].
sem_n(icecreams)-->[icecreams].

% extension of vocabulary
sem_n(book)		-->[book].
sem_n(books)	-->[books].
sem_n(feather)	-->[feather].
sem_n(feathers)	-->[feathers].
sem_n(baby)		-->[baby].
sem_n(babies)	-->[babies].
sem_n(boy)		-->[boy].
sem_n(boys)		-->[boys].
sem_n(girls)	-->[girls].
sem_n(icecream)	-->[icecream].
sem_n(dogs)		-->[dogs].
sem_n(cats)		-->[cats].

sem_n(X)		-->sem_pn(X). % a proper noun is also a noun

/*---------------------------------------------------------------------*/
/*					 Proper Nouns (sem_pn) 				*/
/*---------------------------------------------------------------------*/
% needed for example
sem_pn(victoria)-->[victoria].
sem_pn(katerina)-->[katerina].
sem_pn(james)	-->[james].

% extension of vocabulary
sem_pn(mary)	-->[mary].
sem_pn(john)	-->[john].
sem_pn(tomy)	-->[tomy].

/*---------------------------------------------------------------------*/
/* 						Determiner (det) 					*/
/*---------------------------------------------------------------------*/
% needed for example :
sem_det(the)	-->[the].
sem_det(a)	-->[a].

% extension of vocabulary
sem_det(an)	-->[an].

/*==========================================================*/
/* 			    	END OF SEMANTICS  VOCABULARY		*/
/*==========================================================*/

/*==========================================================*/
/*					KNOWLEDGE BASE SESSION			*/
/*==========================================================*/

/*---------------------------------------------------------------------*/
/* 					update knowledge base				*/
/*---------------------------------------------------------------------*/
update_knowledge_base([]) :- !.
update_knowledge_base([S|Sem]) :- 
   assert(kb_fact(S)),
   write(kb_fact(S)),write(' asserted'),nl,
   update_knowledge_base(Sem), !.

/*---------------------------------------------------------------------*/
/* 					all facts of knowledge base				*/
/*---------------------------------------------------------------------*/
show_kb :- listing(kb_fact/1).

/*---------------------------------------------------------------------*/
/* 			insert additional information to knowledge base		*/
/*---------------------------------------------------------------------*/
/*---------------------------------------------------------------------*/
/* 							examples :					*/
/*---------------------------------------------------------------------*/
/* ?- tell([dog,runs,slowly]).				*/
/* ?- tell([james,is,short]).				*/
/* ?- tell([katerina,walks]).				*/
/*---------------------------------------------------------------------*/
tell(Sentence):- 
	sem(_, Sem, Sentence, []),
	assert(kb_fact(Sem)),
	nl,write(kb_fact(Sem)), nl, write(' added to knowledge base.'),nl, !.

/*---------------------------------------------------------------------*/
/* 						ask knowledge base				*/
/*---------------------------------------------------------------------*/
% Yes-No questions
/*---------------------------------------------------------------------*/
/* examples :					*/
/* ?- ask([does,victoria,love,anime]).			*/
/* ?- ask([is,victoria,tall]).				*/
/* ?- ask([is,the,cat,running]).				*/
/*---------------------------------------------------------------------*/
ask(X):-  q(_, tf,  Sem, X, []), 
	if_then_else(kb_fact(Sem),  write('Yes.'), write('No.') ), !.

/*---------------------------------------------------------------------*/
/* 						yes/no queries					*/
/*---------------------------------------------------------------------*/
q(1,tf,Sem) --> sem_av(does), sem_pn(N), sem_v(V,q), sem_n(N1),  		{Sem=..[V,N,N1]}.
q(1,tf,Sem) --> sem_av(did), sem_pn(N), sem_v(V,q), sem_n(N1),  		{Sem=..[V,N,N1]}.
q(2,tf,Sem) --> sem_av(is), sem_pn(N), sem_adj(A), 						{Sem=..[A,N]}.
q(3,tf,Sem) --> sem_av(does), sem_pn(N), sem_v(V,q), sem_n(N1),  		{Sem=..[V,N,N1]}.
q(4,tf,Sem) --> sem_av(is), sem_pn(N), sem_iv(V,q),						{Sem=..[V,N]}.
q(5,tf,Sem) --> sem_av(is), sem_pn(N), sem_iv(V,q), sem_adv(A),			{Sem=..[V,N,A]}.
q(6,tf,Sem) --> sem_av(does), sem_pn(N),  sem_tv(V,q), sem_pn(N1), sem_np(N2),	{Sem=..[V,N,N1,N2]}. 
q(6,tf,Sem) --> sem_av(does), sem_pn(N), sem_tv(V,q), sem_pn(N1), sem_np(N2),	{Sem=..[V,N,N1,N2]}. 


% 						other questions
/*---------------------------------------------------------------------*/
/* examples :					*/
/* ?- ask([who,loves,icecreams]).				*/
/* ?- ask([what,does,victoria,love]).			*/
/* ?- ask([who,is,tall]).				*/
/* ?- ask([who,is,running]).				*/
/* ?- ask([who,gives,james,cat]).				*/
/* ?- ask([who,is,victoria,giving,a,candy,to]).			*/
/* ?- ask([what,is,victoria,giving,to,katerina]).			*/
/*---------------------------------------------------------------------*/
ask(X):-  q(_,fact, Fact, X, []), write(Fact), !.

/*---------------------------------------------------------------------*/
/* 						fact queries					*/
/*---------------------------------------------------------------------*/
q(1,fact,F) --> [who], sem_v(V,s),sem_n(N1),	  						{Sem=..[V,F,N1], kb_fact(Sem)}.
q(1,fact,F) --> [what],sem_av(does),sem_pn(N),sem_v(V,q),  				{Sem=..[V,N,F], kb_fact(Sem)}.
q(2,fact,F) --> [who], sem_av(is), sem_adj(A), 							{Sem=..[A,F],kb_fact(Sem)}.
q(3,fact,F) --> [who], sem_vp(1,V,N1), 	 								{Sem=..[V,F,N1],kb_fact(Sem)}.
q(3,fact,F) --> [who], sem_av(does),sem_pn(N),sem_v(V,q),  				{Sem=..[V,N,F],kb_fact(Sem)}.
q(4,fact,F) --> [who], sem_av(is),sem_iv(V,q),							{Sem=..[V,F],kb_fact(Sem)}.
q(5,fact,F) --> [how], sem_av(does),sem_pn(N),sem_iv(V,s),				{Sem=..[V,N,F],kb_fact(Sem)}.
q(5,fact,F) --> [how], sem_av(is),sem_pn(N),sem_iv(V,q),				{Sem=..[V,N,F],kb_fact(Sem)}.
q(5,fact,F) --> [who], sem_iv(V,s),sem_adv(A),							{Sem=..[V,F,A],kb_fact(Sem)}.
q(6,fact,F) --> [who], sem_tv(V,s),sem_pn(N1),sem_np(N2),				{Sem=..[V,F,N1,N2],kb_fact(Sem)}.
q(6,fact,F) --> [who], sem_av(is),sem_pn(N),sem_tv(V,q2),sem_np(N2),[to],	{Sem=..[V,N,F,N2] ,Sem}.
q(6,fact,F) --> [what],[is],sem_pn(N),sem_tv(V,q2),[to],sem_pn(N1), 		{Sem=..[V,N,N1,F] ,Sem}.

/*==========================================================*/
/*				END OF KNOWLEDGE BASE SESSION		*/
/*==========================================================*/


/*==========================================================*/
/* 					GENERAL LIBRARY				*/
/*==========================================================*/
/*---------------------------------------------------------------------*/
/* 					 Write Results 					*/
/*---------------------------------------------------------------------*/
write_results([]) :- nl, !.
write_results([H|T]) :- 
	write(H), nl,
	write_results(T).

if_then_else(Condition,A,_):- call(Condition), call(A), !.
if_then_else(_,_,B):- call(B), !.

/*==========================================================*/
/* 					COMMENTS / DEFINITIONS			*/
/*==========================================================*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Πρόσθετα επεξηγηματικά σχόλια						*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Proper Noun 								*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Proper Nouns (proper_noun): Αναφέρεται σε ονόματα και τοποθεσίες. Μπορεί να είναι	*/
/* υποκείμενο αλλά και  αντικείμενο σε μια πρόταση και δεν παίρνει άρθρο. 		*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Verp Phrase 								*/
/* Verb Phrase: Φράσεις που περιέχουν το ρήμα και το αντικείμενο ή κατηγορούμενο 		*/
/* (αν αυτό υπάρχει):								*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Intransitive Verb 								*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Intransitive Verbs (intransitive_verb): Ρήμα που δεν δέχεται αντικείμενο. Αναφέρει πράξη	*/
/* του υποκειμένου  και μπορεί από μόνο του να αποτελεί φράση (verb phrase). 		*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Transitive Verb 								*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Transitive Verbs (transitive_verb): Μεταβατικά ρήματα, δέχονται υποκείμενο, και πάνω	*/
/* από ένα αντικείμενa.  Ένα άμεσο και ένα απλό. Στο παράδειγμα «Ο Κώστας δίνει ένα μήλο	*/
/* στην Μαρία» το μήλο είναι το αντικείμενο και η Μαρία το άμεσο αντικείμενο. Χρησιμοποιείται */
/* ξεχωριστά καθώς στην βάση γνώσης έχει τρία γνωρίσματα  (1 υποκείμενο, 2 αντικείμενα) 	*/
/* και χρησιμοποιούνται τρείς τύποι του. Στην παρούσα έκδοση του προγράμματος υπάρχει μόνο 	*/
/* ένα ρήμα το give:								*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Verb 									*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Verbs (verb): Ο όρος χρησιμοποιείται για όλα τα ρήματα τα οποία δέχονται αντικείμενο.	*/
/* Μεταξύ τους υπάρχουν πολλ οί τύποι ρημάτων (emotion verbs, action verbs, helping verbs).	*/
/* Δίνονται πάλι όπως στα iv δύο τύποι συνδεόμενοι για χρήση σε διαφορετικούς τύπους 	*/
/* προτάσεων. Στην παραγωγή γνώσης παίρνουν δυο ορίσματα, το υποκείμενο και το αντικείμενο	*/
/*---------------------------------------------------------------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Auxiliary Verb 										*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Auxiliary Verbs (auxiliary_verb): Auxiliary verbs ή αλλιώς helping verbs ονομάζονται τα ρήματα	*/
/* που λειτουργούν  ως βοηθητικά στα ρήματα που τα ακολουθούν, δίνοντας τους ή  		*/
/* συμπληρώνοντας το νόημα τους.						*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Noun Phrase								*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Noun Phrase (noun_phrase): Is either a pronoun or any group of words that can be replaced by	*/
/* a pronoun . Φράση που δηλώνει το υποκείμενο σε μια πρόταση. Μπορεί να αποτελείται  	*/
/* και από ένα proper noun 							*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Noun 									*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Noun (noun): Τα ουσιαστικά στο πρόγραμμα χρησιμοποιούνται ως αντικείμενα και μόνο.	*/
/* Τον ρόλο του υποκειμένου παίρνουν συνήθως αλλά όχι αποκλειστικά τα proper nouns.  	*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Adverb 									*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Adverb (adverb): Μέρος του λόγου που περιγράφει ένα επίρρημα, 			*/
/*----------------------------------------------------------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Adjective								*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Adjectives (adjective): Στην ελληνική γλώσσα τα επίθετα, χρησιμοποιούνται ως χαρακτηρισμός	*/
/* προσώπου στο συγκεκριμένο πρόγραμμα. 					*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Determiner								*/
/*---------------------------------------------------------------------------------------------------------------------------------*/
/* Determiner (det): Ονομάζονται οι λέξεις που είτε χρησιμοποιούνται για την κλήση ενός 	*/
/* ουσιαστικού (άρθρα όπως a, the) 						*/
/*---------------------------------------------------------------------------------------------------------------------------------*/






