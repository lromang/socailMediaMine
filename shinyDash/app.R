###########################################
## Author: Luis Manuel Román García
## ----------------------------------------
## Desc:
## Sample Dashboard for Google Places
## API
###########################################

## ----------------------------------------
## Libraries
## ----------------------------------------

## Shiny libraries
library(shiny)
library(shinydashboard)

## Shinyapps
library(rsconnect)

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

## Data frames
library(reshape2)
library(datasets)

## ----------------------------------------
## Functions
## ----------------------------------------
gplaces <- function(lat, lon, r, type, keyword){
    if (length(keyword) > 0){
        a <- paste("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=",
                  lat, ",", lon,
                  "&radius=", r,
                  "&type=", type,
                  "&keyword=", keyword,
                  "&key=AIzaSyCHpy9FreifSRimSSkl4p7DV3wq4pNZ108",
                  sep='')
    }else{
        a <- paste("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=",
                  lat, ",", lon,
                  "&radius=", r,
                  "&type", type,
                  "&key=AIzaSyCHpy9FreifSRimSSkl4p7DV3wq4pNZ108",
                  sep = '')
    }
    document <- RJSONIO::fromJSON(a)$results
    ## Acomodo los resultados en una matriz
    places           <- data.frame(matrix(0, length(document), 8))
    colnames(places) <- c("name", "rating", "type",
                         "dir", "place_id", "lat", "lon",
                         'latlon')
    places$rating    <- NA
    for(i in 1:length(document)){
        places$name[i]     <- document[[i]]$name
        places$place_id[i] <- document[[i]]$place_id
        ## If location available
        if(length(document[[i]]$geometry$location) > 0){
            places$lat[i]    <- document[[i]]$geometry$location[1]
            places$lon[i]    <- document[[i]]$geometry$location[2]
            places$latlon[i] <- paste(places$lat[i],
                                     places$lon[i],
                                     sep = ':')
        }
        ## If rating available
        if(length(document[[i]]$rating) > 0){
            places$rating[i] <- document[[i]]$rating
        }
        ## If type available
        if(length(document[[i]]$types) > 0){
            vec <- c()
            for(j in 1:(length(document[[i]]$types))){
                vec <- paste(vec, document[[i]]$types[[j]], collapse = ' ')
            }
            places$type[i] <- vec
        }
        ## If vicinity available
        if(length(document[[i]]$vicinity)>0){
            places$dir[i] <- document[[i]]$vicinity
        }
    }
    rownames(places) <- 1:nrow(places)
    places
}

## ----------------------------------------
## Front
## ----------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = 'Social Miner'),
    dashboardSidebar(
        h4('Ubicación'),
        textInput('lon', 'Longitud', value = -0.09),
        textInput('lat', 'Latitud', value = 51.505),
        textInput('dir', 'Dirección'),
        h4('Características'),
        textInput('type', 'Tipo de local'),
        textInput('key', 'Productos específicos'),
        sliderInput('radius', 'Radio: ', 1, 20, 5)
    ),
    ## Body
    dashboardBody(
        ## Fluid Row
        fluidRow(
            box(htmlOutput('map', height = 250))
        ) ## Fluid Row End
    ) ## Body End
)

## ----------------------------------------
## Back
## ----------------------------------------
server <- function(input, output){
    output$map <- renderGvis({
        print(input$lat)
        print(input$lon)
        print(paste(input$lat, input$lon, sep = ':'))
        ## Get places matrix
        places <- gplaces(input$lat,
                         input$lon,
                         input$radius,
                         input$type,
                         input$key)
        ## Print places
        print(head(places))
        ## MAP
        gvisMap(places, 'latlon', 'Tip',
                options = list(showTip  = TRUE,
                               showLine = TRUE,
                               enableScrollWheel = TRUE,
                               mapType = 'terrain',
                               useMapTypeControl = TRUE))
    })
}

## ----------------------------------------
## Exec
## ----------------------------------------
shinyApp(ui, server)
