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

## ----------------------------------------
## Front
## ----------------------------------------
ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody()
)

## ----------------------------------------
## Back
## ----------------------------------------
server <- function(input, output){}

## ----------------------------------------
## Exec
## ----------------------------------------
shinyApp(ui, server)
