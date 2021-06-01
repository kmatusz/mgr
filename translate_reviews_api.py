import pandas as pd
import numpy as np
import nltk
import googletrans
from googletrans import Translator
import os
import time
import pickle

df1 = pd.read_csv('data/olist_order_reviews_dataset.csv')

translator = Translator()

if True:
    os.remove('data/translate_dumps.pickle')
    os.remove('data/most_recent_dump.txt')

freq_dump = 100
comments_unique = list(df1.review_comment_message.dropna())
comments_translations = {}

print('starting translations')
time_start = time.time()


for i, element in enumerate(comments_unique):
    # add translation to the dictionary
    comments_translations[element] = translator.translate(element,src='pt').text
    
    if i % freq_dump == 0:
#         time.sleep(2)
        print(i)
        print('time since start: ', str(round(time.time() - time_start)))
        print('content:', str(comments_translations[element][:10]))
        with open('data/translate_dumps.pickle', 'wb') as handle:
            pickle.dump(comments_translations, handle)
        
        with open('data/most_recent_dump.txt', 'a') as file:
            file.writelines([str(i), '\n'])
        
        
