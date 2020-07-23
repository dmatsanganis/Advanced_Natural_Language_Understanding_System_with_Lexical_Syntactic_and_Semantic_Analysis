from nltk.tokenize import word_tokenize, sent_tokenize
def sent_split(documents):
    words = [word_tokenize(sent) for sent in sent_tokenize(text)]
    return words

text = 'Hello all. My name is Titipat, the best LoL player.'
t=sent_split(text)
print(t)
