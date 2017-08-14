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
        iconv("UTF-8", "UTF-8", "")    %>%
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
clean_data <- data.frame(
    'contents'  = toText(data$Contenido),
    'date'      = strptime(data$Fecha.Creación,
                         format = '%m/%d/%Y %H:%M',
                         tz = 'CT'),
    'tags'      = toText(data$Tags),
    'cards'     = data$todas.las.tarjetas,
    'street'    = data$Y,
    'zip.code'  = data$Código.Postal,
    'inBetween' = data$Entre,
    'cash'      = data$Efectivo,
    'nei'       = data$Colonia,
    'corner'    = data$Esquina,
    'type'      = toText(data$Generos),
    'lat'       = data$Latitud,
    'lon'       = data$Longitud,
    'minPrice'  = data$Precio.inferior,
    'maxPrice'  = data$Precio.Superior,
    'mCard'     = data$MasterCard,
    'amex'      = data$Amex,
    'visa'      = data$Visa,
    'numExt'    = data$numExt,
    'numInt'    = data$numInt,
    'schedule'  = data$Horario,
    'services'  = toText(data$Sevicios),
    'state'     = data$Estado,
    'del'       = data$Delegación,
    'street'    = data$Calle,
    'tel'       = data$Telefono.1,
    'web'       = data$Sitio.Web,
)
