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
    
    ## Streamgraph data ----
    df <- readRDS("R dataframe/stream_selected_c_clean.rds")
    stream_gg <- df %>% 
        melt(1:10) %>%  #Keep columns 1 - 9, create a row entry for each column value of 10 - 18
        group_by(year_week,variable) %>% 
        summarise(value = sum(value))

    output$distPlot <- renderPlot({
        cap_space <- function(string){capitalize(str_replace(string,pattern = "_",replacement = " "))}
        ggplot(stream_gg[stream_gg[stream_gg$year_week ---------contienuesas], aes(x = year_week, y = value, fill = cap_space(string = variable))) +
            geom_stream() +
            geom_stream_label(aes(label = cap_space(string = variable))) + 
            theme(panel.background = element_blank()) +
            labs(fill = 'Features')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
