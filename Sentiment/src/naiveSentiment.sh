#! /bin/bash

## Input word
word=$1
nwords=$2
word_vec=''

for i in $(echo $word);
do
    word_vec=$word_vec'\n'$(cat ../data/SBW-vectors-300-min5.txt | grep -E -m 1 "^$i ")
done

## Sentiment vectors: replace with clusters...
good_vec=$(cat ../data/SBW-vectors-300-min5.txt | grep -E -m $nwords "^buen.* ")
bad_vec=$(cat ../data/SBW-vectors-300-min5.txt | grep -E -m $nwords "^mal.* ")

echo "$good_vec" > good.txt
echo "$bad_vec"  > bad.txt
echo -e "$word_vec" > word.txt

./average.R

rm *.txt
