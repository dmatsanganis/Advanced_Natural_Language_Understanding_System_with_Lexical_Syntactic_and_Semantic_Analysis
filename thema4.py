from nltk.tokenize import sent_tokenize, word_tokenize
import sys
example_text = 'the dog needs food. the cat has the food. the dog hates the cat. the dog chased the cat. the cat is scary.'
print(sent_tokenize(example_text))
print(word_tokenize(example_text))