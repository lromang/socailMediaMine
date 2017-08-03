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
    dashboardSidebar(
        menuItem('Dashboard', tabName = 'dashboard', icon = icon('dashboard'))
    ),
    ## Body
    dashboardBody(
        ## Tab Items
        tabItems(
            ## Tab Item: Dashboard
            tabItem(tabName = 'dashboard',
                    fluidRow(
                        box(plotOutput('plot1', height = 250)),
                        box(title       = 'Controls',
                            sliderInput('slider', 'Number of observations', 1, 100, 50))
                    ) ## Fluid Row End
            ) ## Tab Item End: dashboard
        ) ## Tab Items End
    ) ## Body End
)

## ----------------------------------------
## Back
## ----------------------------------------
server <- function(input, output){
    set.seed(122)
    histdata <- rnorm(500)

    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
}

## ----------------------------------------
## Exec
## ----------------------------------------
shinyApp(ui, server)
