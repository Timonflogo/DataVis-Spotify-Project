# Libraries
pacman::p_load(reshape2,ggplot2,ggstream,streamgraph,RColorBrewer,Hmisc,dplyr,gridExtra,shiny,shinythemes
               ,shinyWidgets,plotly,tidyverse,ggalluvial,fmsb,ragtop)

# Commands to run on initiating the app
df <- readRDS("Data/stream_selected_c_clean.rds")
source("ConvenienceFunctions/ConvenienceFunctions.R")
source("VizScripts/Visualizations.R")
source("Data/stream_group_data.R")

categories <- unique(df$artist_id)

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
                 
                 numericInput(inputId = "NumArtists",label = "Number of artists:",value =  10, min = 1, max = 50),
                 numericInput(inputId = "NumTracks",label = "Number of tracks:",value =  10, min = 1, max = 50),
                 
                 plotlyOutput("bar"),
                 
                 uiOutput("back"),
                 
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
    
    
    ### Drill down bar chart ----
    
    current_category <- reactiveVal()
    
    #### Data for the bar chart ----
    
    barplot_data <- reactive({
        
        if (!length(current_category())) {
            return(
                df_barchart <- masterData() %>% 
                    group_by(artist_id) %>% 
                    summarise(msPlayed = sum(msPlayed)) %>%
                    mutate(hsPlayed = msPlayed/3600000) %>% 
                    select(-msPlayed) %>% 
                    arrange(desc(hsPlayed)) %>% 
                    top_n(n = ifelse(test = is.blank(input$NumArtists),yes = 10,no = input$NumArtists))
            )
        }
        
        df_barchart <- masterData() %>% 
            filter(artist_id %in% current_category()) %>% 
            group_by(track_id) %>% 
            summarise(msPlayed = sum(msPlayed)) %>% 
            mutate(hsPlayed = msPlayed/3600000) %>% 
            select(-msPlayed) %>% 
            arrange(desc(hsPlayed)) %>% 
            top_n(n = ifelse(test = is.blank(input$NumTracks),yes = 10,no = input$NumTracks))
    })
    
    output$bar <- renderPlotly({
        d <- setNames(object = barplot_data(),nm = c("x", "y"))
        
        plot_ly(data = d
                ,x = ~reorder(x,desc(y)), y = ~y
                ,type = "bar") %>%
            layout(title = current_category() %||% "Artist") #go for current Artist unless else is selected
        
    })
    
    # update the current category when appropriate
    observe({
        cd <- event_data("plotly_click")$x #x = the label
        if (isTRUE(cd %in% categories)) current_category(cd)
    })
    
    # populate back button if category is chosen
    output$back <- renderUI({
        if (length(current_category())) 
            actionButton("clear", "Back", icon("chevron-left"))
    })
    
    # clear the chosen category on back button press
    observeEvent(input$clear, current_category(NULL))
    
    
    
    
    
    ## Test ----
    
    output$BrushedData <- renderDataTable({
        barplot_data()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)