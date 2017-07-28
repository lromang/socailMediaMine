#! /bin/Rscript

## ---------------------------
## Luis Manuel Román García
##
## Naive sentiment classifier
## ---------------------------

## Load libraries
library(plyr)

## Read in dataset
good <- read.table("good.txt", sep = " ")
bad  <- read.table("bad.txt", sep = " ")
word <- read.table("word.txt", sep = " ")

## --------------------
## Good
## --------------------
## Averaging good words
mean_good <- apply(good[,2:ncol(good)], 2, mean)
## Normalize good words
mean_good <- mean_good/sqrt(sum(mean_good*mean_good))

## --------------------
## Bad
## --------------------
## Averaging bad words
mean_bad <- apply(bad[,2:ncol(bad)], 2, mean)
## Normalize bad words
mean_bad <- mean_bad/sqrt(sum(mean_bad*mean_bad))

## --------------------
## Words
## --------------------
## Averaging words
mean_word <- apply(word[,2:ncol(word)], 2, mean)
## Normalize words
mean_word <- mean_word/sqrt(sum(mean_word*mean_word))

## --------------------
## Cosine distance
## --------------------
bad_like <- sum(mean_word*mean_bad)
good_like <- sum(mean_word*mean_good)

## --------------------
## Classify with margin
## --------------------
epsilon <- .01
if(bad_like > good_like+ epsilon){
    print(paste("Malo", bad_like / good_like, sep = " | confidence: "))
}else if(bad_like + epsilon < good_like){
    print(paste("Bueno", good_like / bad_like, sep = " | confidence: "))
}else{
    print(paste("Neutro", max(good_like, bad_like)/min(good_like, bad_like), sep = " | confidence: "))
}
