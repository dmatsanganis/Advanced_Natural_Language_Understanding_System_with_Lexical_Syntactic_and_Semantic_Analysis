/*----------------------------------------------------------------------*/
/* 		PROJECT 	ON			*/
/* 	NATUTAL LANGUAGE PROCESSING (NLP)	*/
/*----------------------------------------------------------------------*/
/* Author :Themis Panayiotopoulos			*/
/* June 2020					*/
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
/*	?- understand(‘text.txt’),			*/
/*	?- understand(‘maria.txt’),			*/
/*----------------------------------------------------------------------*/
/* story in text.txt :					*/
/* the dog needs food. the cat has the food. the dog hates the 	*/
/* cat. the dog chased the cat. the cat is scary.		*/
/*----------------------------------------------------------------------*/
/* story in maryt.txt :				*/
/* mary runs quickly. mary is tall. mary is slim. mary is blonde.	*/
/* mary gives john a dog. mary gives tomy a book. 		*/
/* mary loves books 					*/
/*----------------------------------------------------------------------*/
:- discontiguous ask/1, q/5.
/*----------------------------------------------------------------------*/
/* vocabulary elements of the text.txt example		*/
/*----------------------------------------------------------------------*/
:- dynamic chased/2, hates/2, has/2, needs/2, scary/1.

/*----------------------------------------------------------------------*/
/* aditional vocabulary elements	 (also for mary.txt example)	*/
/*----------------------------------------------------------------------*/
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

lexical_analyse(Text, Result) :- 
	see(Text),         		 /* open this file to read */
	read(X),			/* read from File */ 
	seen,			/* close the input File */ 
	analyse(X,Result),		/* Lexical Analysis */
	write_results(Result),	/* Wite Result */
	!.			/* stop now */ 


analyse(end_of_file,[]) :- !. 

analyse(Input,All) :- input_to_sentences(Input,All).

input_to_sentences(Input, List_of_Words):- 
	name(Input,Ascii_List), 
	tokenise(Ascii_List, List_of_Words).


tokenise([],[]):-!.

tokenise(Ascii_List,All):-
	one_sentence(Ascii_List, Sentence, Rest, T),
	T=end_of_sentence,
	tokenise(Rest, List_of_Sentences),
	append([Sentence],List_of_Sentences,All).


one_sentence(Ascii_List, [Word], Rest_Ascii, end_of_sentence):-
   one_word(middle_of_word, Ascii_List, Ascii_Word, Rest_Ascii, T),
   T=end_of_sentence,
   name(Word,Ascii_Word), !.

one_sentence([], [], [], end_of_text):- !. 


one_sentence(Ascii_List, Sentence, Rest_Text, Type):-
   one_word(start_of_word, Ascii_List, Ascii_Word, Rest_Ascii, T),
   T=end_of_word,
   name(Word,Ascii_Word),
   one_sentence(Rest_Ascii, Rest_Words, Rest_Text, Type),
   append([Word], Rest_Words, Sentence).


one_word(middle_of_word, [], [], [], end_of_text):-  !.
one_word(middle_of_word, [46|T], [], T, end_of_sentence):-  !. 
one_word(middle_of_word, [32|T], [], T, end_of_word):-  !. 

one_word(Any, [13|T], Word, Rest_Codes, Type):- 
	one_word(Any, T,  Word,  Rest_Codes, Type).
one_word(Any, [10|T], Word, Rest_Codes, Type):- 
	one_word(Any, T,  Word,  Rest_Codes, Type).
one_word(start_of_word, [32|T], Word, Rest_Codes, Type):- 
	one_word(start_of_word, T,  Word,  Rest_Codes, Type).
one_word(_, [Ascii_Code|T], [Ascii_Code|Word], Rest_Codes, Type):-
	Ascii_Code \= 32,
	one_word(middle_of_word, T, Word, Rest_Codes, Type).




