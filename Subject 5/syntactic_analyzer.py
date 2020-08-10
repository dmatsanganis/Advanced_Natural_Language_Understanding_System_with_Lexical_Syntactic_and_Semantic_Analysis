import nltk
grammar = nltk.CFG.fromstring("""
  S  -> NP VP
  NP -> PN | Det N | N
  VP -> IV | IV Adv | AV Adj | TV PN NP | V NP
  IV -> 'runs' | 'run' | 'running' | 'hurts' | 'hurt' | 'hurting' | 'walks' | 'walk' | 'walking' | 'jumps' | 'jump' | 'jumping' | 'shoots' | 'shoot' | 'shooting'
  AV -> 'is' | 'are' | 'does' | 'do' 
  TV -> 'gives' | 'give' | 'gave' | 'giving'
  PN -> 'mary' | 'john' | 'tomy' 
  Adv -> 'quickly' | 'slowly' | 'independently'
  Det -> 'the' | 'a' | 'an'
  N -> 'food' | 'cat' | 'dog' | 'dogs' | 'cat' | 'cats' | 'book' | 'books' | 'feather' | 'feathers' | 'baby' | 'babies' | 'boy' | 'boys' | 'girls' | 'girl' | 'icecream' | 'icecreams'
  Adj  -> 'scary' | 'tall' |  'short' | 'blonde' | 'slim' | 'fat'
  V ->  'chased'  | 'chase' | 'needs' | 'hates' | 'hate' | 'has' | 'has' | 'have' | 'loves' | 'love' | 'kicks' | 'kick' | 'jumps' | 'jump'
  """)
sentence = "mary gave john a book"
sent=sentence.split()
rd_parser = nltk.RecursiveDescentParser(grammar)
print("Parsing  the sentence:"+ " " + sentence)
for tree in rd_parser.parse(sent):
    print("The tree for the above sentence is:")
    print(tree)
    break
