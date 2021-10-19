# Libraries
pacman::p_load(reshape2,ggplot2,ggstream,streamgraph,RColorBrewer,Hmisc,dplyr,gridExtra,shiny,shinythemes
               ,shinyWidgets,plotly,tidyverse,ggalluvial,fmsb)

# Commands to run on initiating the app
df <- readRDS("Data/stream_selected_c_clean.rds")
source("ConvenienceFunctions/ConvenienceFunctions.R")
source("VizScripts/Visualizations.R")
source("Data/stream_group_data.R")

# UI ----
ui <- navbarPage(
    theme = shinytheme("cerulean"),
    fluid = TRUE, 
    "Spotify streaming",
    
    ## Tab 1 - Welcome -----
    tabPanel(title = "Welcome",
             h3("Spotify"),
             p("This is some text introduction"),
             
             # Main panel for page 1
             mainPanel(
                 plotOutput(outputId = "Streamgraph", 
                            brush = brushOpts(id = "plot1_brush", 
                                              direction = "x"
                            )
                 ),
                 
                 plotOutput(outputId = "Ribbonchart"),
                 
                 plotOutput(outputId = "Radarchart"),
                 
                 textOutput(outputId = "FilterText"),
                 
                 dataTableOutput(outputId = "BrushedData"),
                 
                 width = 11
             )
    )
)

# Server ----
server <- function(input,output){
    
    ## Getting data ----
    
    ### masterData ----
    masterData <- reactive({
        
        df <- readRDS("Data/stream_selected_c_clean.rds")
        data <- brushedPoints(df = stream_group_date,brush = input$plot1_brush)
        if (nrow(data) == 0)
            data <- stream_group_date
        
        filter_start_date <<- sort(data$date,decreasing = FALSE)[1] # <<- to assign to global env.
        filter_end_date <<- sort(data$date,decreasing = TRUE)[1] # <<- to assign to global env.
        
        #Return the data
        df[date >= filter_start_date & date <= filter_end_date,]
        
    })
    
    ### Filter Text ----
    output$FilterText <- renderText({
        masterData() #To activate the master data function
        paste0("You have filtered your data from ",filter_start_date," to ",filter_end_date)
    })
    
    ## Viz ----
    
    ### Streamgraph ----
    output$Streamgraph <- renderPlot({
        streamgraph(dataInput1 = stream_group_date)
    }
    )
    
    ### Ribbon chart ----
    
    output$Ribbonchart <- renderPlot({
        ribbonchart(dataInput1 = masterData())
    })
    
    ### Radar chart ----
    
    output$Radarchart <- renderPlot({
        radarplot(dataInput1 = masterData())
    })
    
    ## Test ----
    
    output$BrushedData <- renderDataTable({
        masterData()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)