#!/bin/env python3
# NEED TO CONSIDER SPECIAL CHARS + ELLIPSIS!!!
import sys

# OPEN FILE
try:
    filename = sys.argv[1]
    f = open(filename)
except:
    print('ERROR: please give me a file')
    sys.exit(1)

# GEN DATA
d_words = {}
with f:
    for line in f:
       for word in map(str.lower, line.split()):
            d_words[word] = 1 if word not in d_words else d_words[word]+1
d_lengths = {}
for word, count in d_words.items():
    d_lengths[len(word)] = count if len(word) not in d_lengths else d_lengths[len(word)] + count

# GEN STATS
tot = sum(d_lengths.values())
if tot == 0:
    print('ERROR: file is empty')
    sys.exit(1)
avg = sum(k*v for k,v in d_lengths.items()) // sum(d_words.values())
stat = sorted(d_lengths.items())
popular = list(sorted(d_words.items(), key=lambda t:t[1], reverse=True))[:10]

# OUTPUT
print(f'Total Words: {tot}\n')
print(f'Average Length: {avg}\n')
print('Top Lengths:')
for l,n in stat:
    print(f'{l:3} chars => {n:5}')
print('\nTop Words:')
for i,(w,n) in enumerate(popular, start=1):
    print(f'{i:2}. {w:10} => {n:5}')
