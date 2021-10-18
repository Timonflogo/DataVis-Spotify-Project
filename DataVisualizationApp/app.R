# Libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(tidyverse)


# Define UI for application that draws a histogram ----
# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme("cerulean"),
    fluid = TRUE, 
    "Movie recommender system",
    
    #' *Tab 1*
    tabPanel(title = "Welcome",
             h3("Spotify of making movie recommendations"),
             p("This is some text introduction"),
    ),
    
    
    #' *Tab 2: Streaming history* 
    tabPanel(title = "Streaming history statistics",
             h3("Spotify streaming history"),
             p("This is your streaming history."),
             
             #Create fluid row to set inputs side-by-side
             fluidRow(
                 column(width = 2,
                        sliderInput("range", "Time period:",
                                    min = 1, max = 53,
                                    value = c(1,53))
                        ,
                 )
             ),
             fluidPage(
                 # column(width = 2,
                 #        numericInput(inputId = "no_recc_euc",label =  "Euc. dist recommendations:"
                 #                     ,value =  5,min = 1,max = 10),
                 # ),
                 # 
                 # 
                 # column(width = 2,
                 #        numericInput(inputId = "no_recc_lda",label =  "Topic recommendations:"
                 #                     ,value =  5,min = 1,max = 10),
                 # ),
                 # 
                 mainPanel(
                     tableOutput(outputId = "distPlot"),
                     width = 11
                 )
             )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$weeks + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
