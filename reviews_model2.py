# To add a new cell, type '# %%'
# To add a new markdown cell, type '# %% [markdown]'
# %%
import pandas as pd
import numpy as np


# %%
df_full = pd.read_csv('data/reviews_with_en.csv').drop('Unnamed: 0', axis = 1)

df = df_full[['message_en']].dropna()
df


# %%
df.message_en = df.message_en.fillna(' ')
# %%


from sklearn.decomposition import LatentDirichletAllocation
from wordcloud import WordCloud
from sklearn.feature_extraction.text import TfidfTransformer,CountVectorizer
from sklearn.metrics import roc_auc_score


# %%
from nltk.corpus import stopwords
# nltk.download('stopwords')
stop_words = stopwords.words('english')
a = df['message_en']
# %%

#liked
count_vect = CountVectorizer(min_df=0.001, stop_words=stop_words)
doc_term_matrix = count_vect.fit_transform(df['message_en'].values)
# %%

LDA_liked = LatentDirichletAllocation(n_components=5, random_state=88)
LDA_liked.fit(doc_term_matrix)
# %%

for i,topic in enumerate(LDA_liked.components_):
    print(f'Top 10 words for topic #{i}:')
    print([count_vect.get_feature_names()[i] for i in topic.argsort()[-10:]])
    print('\n')


# LDA is shit - nothing interesting here
# %%

# Trying kmeans
from sklearn.cluster import KMeans

km = KMeans(n_clusters=3)
km.fit(doc_term_matrix)
# %%
df_cl = pd.DataFrame({'message_en':df['message_en'], 'cluster':km.predict(doc_term_matrix)})
# %%
df_cl.query('cluster ==1')

# %%


# Biterm topic model

# get bigrams
from biterm.utility import vec_to_biterms

vocab = np.array(count_vect.get_feature_names())
biterms = vec_to_biterms(doc_term_matrix[:1000,:])
# %%

from biterm.cbtm import oBTM

btm = oBTM(num_topics=3, V=vocab)
topics = btm.fit_transform(biterms, iterations=100)
# %%
topics.shape
# %%


# Find subjects of sentences

import spacy

nlp = spacy.load("en_core_web_sm")
doc = nlp("Apple is looking at buying U.K. startup for $1 billion")

for token in doc:
    print(token.text, token.lemma_, token.pos_, token.tag_, token.dep_,
            token.shape_, token.is_alpha, token.is_stop)
# %%

sub_toks = [tok for tok in doc if (tok.dep_ == "nsubj") ]

print(sub_toks) 
# %%

df.head()
# %%

doc = nlp(list(df.message_en)[4])
# %%
for token in doc:
    print(token.text, token.lemma_, token.pos_, token.tag_, token.dep_,
            token.shape_, token.is_alpha, token.is_stop)
# %%


for token in doc:
    print(token.lemma_, token.pos_)
# %%

for i in list(df.message_en)[:30]:
    print(i)
    doc = nlp(i)
    for token in doc:
        if token.pos_ in ['VERB', 'ADJ', 'NOUN', 'ADV']:
            print(token.lemma_, token.pos_)
        # print(token.text, token.lemma_, token.pos_, token.tag_, token.dep_,
        #     token.shape_, token.is_alpha, token.is_stop)

    print('')

# %%

# Try gibbs sampling model

# %%

in_sent = list(df.message_en)#[:1000] 
out_sent = []
temp_vocab_size= []

for i in in_sent:
    temp = []
    # print(i)
    doc = nlp(i)
    for token in doc:
        if token.pos_ in ['VERB', 'ADJ', 'NOUN', 'ADV']:
            # print(token.lemma_, token.pos_)
            temp.append(token.lemma_)
            temp_vocab_size.append(token.lemma_)

        # print(token.text, token.lemma_, token.pos_, token.tag_, token.dep_,
        #     token.shape_, token.is_alpha, token.is_stop)
    out_sent.append(set(temp))
    # print('')
# %%
len(set(temp_vocab_size))
out_sent
# %%
from gsdmm.gsdmm.mgp import MovieGroupProcess
# %%
mgp = MovieGroupProcess(K=10, alpha=0.1, beta=0.01, n_iters=30)
# %%
y = mgp.fit(out_sent,vocab_size=1146)
# %%
out_tuple = list(zip(y, in_sent))
# %%

for topic_no in set(y):
    print(f'Topic {topic_no}')
    counter = 1
    for i in out_tuple:
        if i[0]==topic_no and counter < 100:
            print(i[1])
            counter += 1

    print('')
# %%

out_sent
# %%

# frequency per topic 
# frequency overall

out_sent
# %%



count_vect2 = CountVectorizer(min_df=0.001, stop_words=stop_words)
doc_term_matrix2 = count_vect2.fit_transform([' '.join(i) for i in out_sent])
# %%
pd.DataFrame({'a':doc_term_matrix2.todense().sum(axis=0).transpose()})


# %%
count_vect2.get_feature_names() 
# %%

frequencies = pd.DataFrame({'word':count_vect2.get_feature_names(), 'occurence_total':doc_term_matrix2.todense().sum(axis=0).tolist()[0]})
# %%

frequencies.sort_values('occurence_total',ascending=False)
# %%


len(y)
# %%
doc_term_matrix2.shape
# %%
y
# %%
for idx, i in enumerate(y):
    print(idx)

# %%

topic_no = 0

indexes_for_topic = []
for idx, i in enumerate(y):
    if i == topic_no:
        indexes_for_topic.append(idx)

# %%

frequencies_topic = pd.DataFrame({'word':count_vect2.get_feature_names(), 'occurence_total':doc_term_matrix2[indexes_for_topic,:].todense().sum(axis=0).tolist()[0]})

# %%

for topic_no in set(y):
    indexes_for_topic = []

    for idx, i in enumerate(y):
        if i == topic_no:
            indexes_for_topic.append(idx)


    frequencies_topic = pd.DataFrame({'word':count_vect2.get_feature_names(), 'occurence_total':doc_term_matrix2[indexes_for_topic,:].todense().sum(axis=0).tolist()[0]})
    frequencies['occurences_topic_'+str(topic_no)] = doc_term_matrix2[indexes_for_topic,:].todense().sum(axis=0).tolist()[0]
# %%
def perc(x):
    return x/sum(x)

frequencies2 = frequencies.select_dtypes(include=['int64']).apply(perc)
# %%
frequencies2['word'] = frequencies.word
frequencies2['occurence_total_num'] = frequencies.occurence_total
# %%


import seaborn as sns
sns.set_theme(style="whitegrid")
# %%
frequencies3=frequencies2.melt(value_vars=frequencies2.select_dtypes(['float']).columns, id_vars = 'word')

# %%
frequencies3
# %%


words_to_show = list(frequencies.sort_values('occurence_total',ascending=False).head(20).word)
# %%
frequencies4=frequencies3[frequencies3.word.isin(words_to_show)]#.query('word in !words_to_show')
# %%
g = sns.catplot(
    data=frequencies4, kind="bar",
    y="word", x="value", hue="variable",
    palette="dark", alpha=.6, height=6
)
# %%
sns.catplot(
    data=frequencies4, kind="bar",
    y="word", x="value", hue="variable",
    palette="dark", alpha=.6, height=6
)
# %%



count_vect_small = CountVectorizer(min_df=0.01, stop_words=stop_words)
doc_term_matrix_small = count_vect_small.fit_transform(df['message_en'].values)
doc_term_matrix_small
# %%
from sklearn.decomposition import PCA, TruncatedSVD
# %%
p = PCA(n_components=2)
p.fit(doc_term_matrix_small.todense())
p_data = p.transform(doc_term_matrix_small.todense())
# %%
import matplotlib.pyplot as plt
p_data
# %%
plt.plot(p_data[:,0], p_data[:,1], 'o')
# %%


out = pd.DataFrame({"message_en":df['message_en'], 
'x1':p_data[:,0], 'x2':p_data[:,1]})
# %%

out.to_csv('review_reduction.csv')
# %%

# Very basic EDA of words in the corpus
df_occurences = df.copy()
df['message_en']
df['message_en'].str.contains('deadline')
# df['message_en'].str.contains('delivery')
# df['message_en'].str.contains('product')

# list(df['message_en'].str.split(' '))


# %%
list(df['message_en'][100:120])



# %%
import spacy
from nltk import Tree


en_nlp = spacy.load('en_core_web_sm')

doc = en_nlp(df['message_en'][110])

def to_nltk_tree(node):
    if node.n_lefts + node.n_rights > 0:
        return Tree(node.orth_, [to_nltk_tree(child) for child in node.children])
    else:
        return node.orth_


[to_nltk_tree(sent.root).pretty_print() for sent in doc.sents]
# %%
df_occurences.assign(a = lambda x: x['message_en'].str.contains('deadline'))
# %%
