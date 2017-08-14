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

## URL
library(httr)
library(httpuv)

## Text
library(stringr)
library(tau)
library(tm)

## Clustering
library(cluster)
library(kernlab)
library(dbscan)
library(EMCluster)
library(clValid)
library(fpc)


## Dimensionality reduction
library(Rtsne)

## ----------------------------------------
## Functions
## ----------------------------------------

## To Text
toText <- function(messages){
    messages <- removeNumbers(messages)    %>%
        tolower()                         %>%
        removePunctuation()               %>%
        str_replace_all("\t", "")         %>%
        iconv("UTF-8", "UTF-8", "")       %>%
        removeWords(stopwords("spanish")) %>%
        str_replace_all("[^[[:print:]]]",
                        "")               %>%
        str_replace_all(" +", " ")
    messages
}

toTextList <- function(tags){
    ## Clean tags
    clean_tags   <- laply(tags,
                         function(t) t <- paste(toText(str_split(t,
                                                                ',')[[1]]),
                                               collapse = " "
                                               )
                         )
    clean_tags
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
    'title'     = toText(data$Titulo),
    'subTit'    = toText(data$Subtítulo),
    'date'      = strptime(data$Fecha.Creación,
                           format = '%m/%d/%Y %H:%M',
                           tz = 'CT'),
    'contents'  = toText(data$Contenido),
    'tags'      = toTextList(data$Tags),
    'type'      = toText(data$Generos),
    'services'  = toTextList(data$Sevicios),
    'schedule'  = data$Horario,
    'minPrice'  = data$Precio.inferior,
    'maxPrice'  = data$Precio.Superior,
    'cash'      = data$Efectivo,
    'cards'     = data$todas.las.tarjetas,
    'mCard'     = data$MasterCard,
    'amex'      = data$Amex,
    'visa'      = data$Visa,
    'state'     = data$Estado,
    'del'       = data$Delegación,
    'quarter'   = toText(data$Colonia),
    'zip.code'  = data$Código.Postal,
    'street'    = toText(data$Calle),
    'corner'    = toText(data$Esquina),
    'inBetween' = toText(data$Entre),
    'numExt'    = data$NumExt,
    'numInt'    = data$NumInt,
    'lat'       = data$Latitud,
    'lon'       = data$Longitud,
    'tel'       = data$Telefono.1,
    'web'       = data$Sitio.Web,
    'score'     = data$Calificación
)
clean_data$state[clean_data$state == 'Distrito Federal'] <- 'Ciudad de México'

## ----------------------------------------
## All text
## ----------------------------------------

## Paste Textual Variables
all_text <- paste(clean_data$subTit,
                 clean_data$contents,
                 clean_data$tags,
                 clean_data$services,
                 sep = ' ')

## Unique places
clean_data <- clean_data[!duplicated(all_text), ]

## Get Word Term Matrix
frex  <- 2
words <- tokenize(all_text)
words <- words[str_length(words) > 2]
words <- removeNumbers(words)
words <- plyr::count(words)
words <- words[!is.na(words$x),]
words <- words[words$freq > frex,]
term_matrix <- data.frame(matrix(0,
                                nrow = length(unique(all_text)),
                                ncol = nrow(words)),
                        row.names    = unique(all_text))
colnames(term_matrix) <- words$x
## Matrix fill-in
for(i in 1:length(unique(all_text))){
  for(j in 1:ncol(term_matrix)){
      term_matrix[i, j] <- str_count(rownames(term_matrix)[i],
                                    colnames(term_matrix)[j])
  }
}

## Matrix without text
c_data_nt <- dplyr::select(clean_data,
                          -subTit,
                          -contents,
                          -tags,
                          -services,
                          -date)
## NA <- -1
c_data_nt[is.na(c_data_nt)] <- -1

## all data
all_data <- cbind(c_data_nt,
                 term_matrix)

## Clustering
clust_dist <- daisy(all_data,
                   metric = 'gower',
                   type   = list(logratio = 3))

## TSNE visualization
tsne_data <- Rtsne(clust_dist, is_distance = TRUE)

tsne_data <- tsne_data$Y   %>%
    data.frame()          %>%
    setNames(c('X', 'Y'))

## Spectral Clustering
sc_cluster <- as.factor(specc(as.matrix(tsne_data), centers = 6))

## plot with clusters
scPlot <- ggplot(data = tsne_data,
       aes(x = X,
           y = Y,
           col  = sc_cluster,
           size = all_data$score)) +
    geom_point() +
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#1972b9"),
          legend.title = element_text(face = "bold",
                                    color = "#424242"),
          panel.grid.major = element_line(colour = "#BDBDBD",
                                          linetype = "dotted"),
          panel.grid.minor = element_line(colour = "#E0E0E0",
                                          linetype = "dotted")) +
    ylab("V2") + xlab("V1") +
    scale_colour_discrete(name = "Clusters")
print(scPlot)
ggsave('../graphs/spectral_clustering.png', scPlot)

## plot with score
scorePlot <- ggplot(data = tsne_data,
       aes(x = X,
           y = Y,
           col  = all_data$score)) +
    geom_point() +
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#1972b9"),
          legend.title = element_text(face = "bold",
                                    color = "#424242"),
          panel.grid.major = element_line(colour = "#BDBDBD",
                                          linetype = "dotted"),
          panel.grid.minor = element_line(colour = "#E0E0E0",
                                          linetype = "dotted")) +
    ylab("V2") + xlab("V1") +
    scale_colour_discrete(name = "Calificación")
print(scorePlot)
ggsave('../graphs/spectral_clustering_score.png', scorePlot)

## SAVE DATA
c_data_nt$cluster <- sc_cluster
write.csv(c_data_nt, '../outputData/clustered_data.csv', row.names = FALSE)
