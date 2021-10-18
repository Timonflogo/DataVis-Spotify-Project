# Libraries
# require("remotes")
# remotes::install_github("hrbrmstr/streamgraph")
# pacman::p_load(reshape2,ggplot2,ggstream,plotly,streamgraph,RColorBrewer,Hmisc,dplyr,gridExtra)
# pacman::p_load(shiny, shinythemes, shinyWidgets, plotly, tidyverse)


# Define a Streamgraph displaying SPotify audio features over time 
# Load Data ----
df <- readRDS("R dataframe/stream_selected_c_clean.rds")
print(df)
df

stream_gg <- df %>% 
    melt(1:10) %>%  #Keep columns 1 - 9, create a row entry for each column value of 10 - 18
    group_by(week_number,variable) %>% 
    summarise(value = sum(value))


ui <- navbarPage(
    theme = shinytheme("cerulean"),
    fluid = TRUE, 
    "Movie recommender system",
    
    #' *Tab 1*
    tabPanel(title = "Welcome",
             h3("Spotify of making movie recommendations"),
             p("This is some text introduction"),
             
             mainPanel(plotOutput(outputId = "Streamgraph"), width = 11)),
    
    #' *Tab 2: Streaming history* 
    tabPanel(title = "Streaming history statistics",
             h3("Spotify streaming history"),
             p("This is your streaming history."),
             
             #Create fluid row to set inputs side-by-side
             fluidRow(
                 column(width = 2,
                        sliderInput(inputId = "weeks",label = "Select weeks:"
                                    ,min = 1,max = 52,value = 1
                                    ,width = 700),
                 )
                 
             )
    )
)

server <- function(input,output){
    
    stream_plot <- reactive({
        stream_gg
    })
    
    
    output$Streamgraph<-renderPlot({
        ggplot(stream_gg, aes(x = week_number, y = value, fill = variable)) +
            geom_stream()}
    )
}


# Run the application 
shinyApp(ui = ui, server = server)