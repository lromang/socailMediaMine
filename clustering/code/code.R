##################################################
## Clustering Businessess
## -----------------------------------------------
## This code enables business clustering based
## on distinct characteristics.
##################################################

## ----------------------------------------
## Libraries
## ----------------------------------------
## Vis
library(googleVis)
library(ggplot2)

## Map
library(ggmap)
library(rMaps)
library(leaflet)

## General purpose
library(readr)

## String parsing
library(stringr)

## JSON
library(RJSONIO)
library(jsonlite)

## Data frames
library(reshape2)
library(datasets)
library(plyr)
library(dplyr)

## YELP
library(yelpr)

## URL
library(httr)
library(httpuv)

## Text
library(stringr)
library(tau)
library(tm)

## ----------------------------------------
## Functions
## ----------------------------------------

## To Text
toText <- function(messages){
    messages <- removeNumbers(messages) %>%
        tolower()                      %>%
        removePunctuation()            %>%
        str_replace_all("\t", "")      %>%
        ## iconv("UTF-8", "ASCII", "")    %>%
        removeWords(stopwords("spanish"))
    messages
}

## ----------------------------------------
## Read in data
## ----------------------------------------
data <- read.csv('../data/bas.csv',
                stringsAsFactors = FALSE)

## ----------------------------------------
## Processing and cleaning variables
## ----------------------------------------
contents <- toText(data$Contenido)
