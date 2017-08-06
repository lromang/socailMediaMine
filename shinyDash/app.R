###########################################
## Author: Luis Manuel Roman Garcia
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
library(DT)
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
library(jsonlite)

## Data frames
library(reshape2)
library(datasets)

## YELP
library(yelpr)

## URL
library(httr)
library(httpuv)

## ----------------------------------------
## Functions
## ----------------------------------------

## Gplaces details
gplaces_details <- function(M){
    ## Este codigo se corre cuando ya tenemos el numero de id_place
    M$telefono   <- NA
    M$horario    <- NA
    M$web        <- NA
    M$comentario <- NA
    for (i in 1:nrow(M)){
        b    <- paste("https://maps.googleapis.com/maps/api/place/details/json?placeid=",
                     M$place_id[i],
                     "&key=AIzaSyCHpy9FreifSRimSSkl4p7DV3wq4pNZ108",
                     sep='')
        detail <- RJSONIO::fromJSON(b)$result
        ## Nos quedamos con aquellos datos que existan
        if(length(detail$formatted_phone_number) > 0){
            M$telefono[i] <- detail$formatted_phone_number
        }
        if(length(detail$opening_hours$weekday_text[1])>0){
            M$horario[i] <- detail$opening_hours$weekday_text[1]
        }
        if(length(detail$reviews[[1]]$text)){
            M$web[i] <- detail$reviews[[1]]$text
        }
        if(length(detail$website)>0){
            M$comentario[i] <- detail$website
        }
    }
    M
}

## GPlaces
gplaces <- function(lat, lon, r, type, keyword = NULL){
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
                  "&type=", type,
                  "&key=AIzaSyCHpy9FreifSRimSSkl4p7DV3wq4pNZ108",
                  sep = '')
    }
    document <- RJSONIO::fromJSON(a)$results
    ## Acomodo los resultados en una matriz
    places           <- data.frame(matrix(0, length(document), 8))
    colnames(places) <- c("name", "rating", "type",
                         "dir", "place_id", "lat", "lon")
    places$rating    <- NA
    for(i in 1:length(document)){
        places$name[i]     <- document[[i]]$name
        places$place_id[i] <- document[[i]]$place_id
        ## If location available
        if(length(document[[i]]$geometry$location) > 0){
            places$lat[i]    <- document[[i]]$geometry$location[1]
            places$lon[i]    <- document[[i]]$geometry$location[2]
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
        textInput('lon', 'Longitud', value = -99.171418),
        textInput('lat', 'Latitud',  value = 19.404068),
        textInput('dir', 'Dirección'),
        h4('Características'),
        textInput('type', 'Tipo de local', value = 'restaurant'),
        textInput('key', 'Productos específicos'),
        sliderInput('radius', 'Radio: ', 1, 20, 5)
    ),
    ## Body
    dashboardBody(
        ## Fluid Row
        fluidRow(
            ## Box
            box(
                title  = 'Places',
                status = 'primary',
                solidHeader = TRUE,
                ## DIV
                div(
                    leafletOutput('map'),
                    style = 'height: 100%; width: 100%'
                ) ## Div End
            ), ## Box End
            box(
                title       = 'Tweets',
                status      = 'primary',
                solidHeader = TRUE,
                htmlOutput('mapbox')
            )
        ), ## Fluid Row End
        div(
            DT::dataTableOutput('table'),
            style='height: 50%; width: 100%;'
        )
    ) ## Body End
)

## ----------------------------------------
## Back
## ----------------------------------------
server <- function(input, output){
    ## MAP
    output$map <- renderLeaflet({
        lat <- input$lat
        lon <- input$lon
        ## In case there is a direction
        if(input$dir != ''){
            ## Geocode
            coords <- geocode(input$dir)
            ## Coords
            lat    <- coords[2]
            lon    <- coords[1]
        }
        ## Get places matrix
        places <- gplaces(lat     = lat,
                         lon     = lon,
                         r       = input$radius * 1000,
                         type    = input$type,
                         keyword = input$key)
        places  <- gplaces_details(places)
        ## Print places
        print(head(places))
        ## MAP
        leaflet() %>%
            addProviderTiles(providers$Stamen.Terrain,
                             options = providerTileOptions(noWrap = TRUE)
                             ) %>%
            setView(mean(places$lon), mean(places$lat), zoom = 13) %>%
            addMarkers(data = places, popup = paste(places$name,
                                                    places$type,
                                                    places$comentario,
                                                    sep = ' '))
    })
    ## TABLE
    output$table <- renderDataTable({
        lat <- input$lat
        lon <- input$lon
        ## In case there is a direction
        if(input$dir != ''){
            ## Geocode
            coords <- geocode(input$dir)
            ## Coords
            lat    <- coords[2]
            lon    <- coords[1]
        }
        ## Get places matrix
        places <- gplaces(lat     = lat,
                         lon     = lon,
                         r       = input$radius * 1000,
                         type    = input$type,
                         keyword = input$key)
        places <- gplaces_details(places)
        ## Places
        print(head(places))
        places
    }, options = list(scrollX    = TRUE,
                      pageLength = 3)
    )
    ## FRAME
    output$mapbox <- renderUI({
        tags$iframe(src='https://igncazares.carto.com/builder/231f8fcb-800d-4ddb-a891-10b7f0911e64/embed',
                    height=390, width = 500)
    })
}

## ----------------------------------------
## Exec
## ----------------------------------------
shinyApp(ui, server)
