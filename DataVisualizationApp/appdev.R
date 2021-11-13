# Libraries
pacman::p_load(reshape2,ggplot2,ggstream,streamgraph,RColorBrewer,Hmisc,dplyr,gridExtra,shiny,shinythemes
               ,shinyWidgets,plotly,tidyverse,ggalluvial,fmsb,ragtop,lubridate)

# Commands to run on initiating the app
df <- readRDS("Data/stream_selected_c_clean.rds")
source("ConvenienceFunctions/ConvenienceFunctions.R")
source("VizScripts/Visualizations.R")
source("VizScripts/Streamgraph.R")
source("VizScripts/Summary_stats.R")
source("Data/stream_group_data.R")
source("VizScripts/Scatter_plot_matrix_graph.R")
source("VizScripts/1DHeatmap.R")
source("VizScripts/mod_linechart.R")
source("VizScripts/mod_radarchart.R")

#Objects for plotly observable events for drilldown
categories <- unique(df$artistName)
weekdays <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

setBackgroundColor(
  color = "ghostwhite",
)

# UI ----
ui <- navbarPage(
  theme = shinytheme("cerulean"),
  fluid = TRUE, 
  "Spotify streaming",
  setBackgroundColor("#E6E6E6"),
  
  # tabPanel(title = "Welcome",
  #          h1("Introduction"),
  #          
  #          # textOutput(outputId = "introText"),
  # 
  #          p(),
  #          
  #          HTML('+ Energy - The energy of a song - the higher the value, the more energtic song <br> 
  #                + Danceability - The higher the value, the easier it is to dance to this song. <br> 
  #                + Loudness (dB) - The higher the value, the louder the song. <br> 
  #                + Liveness - The higher the value, the more likely the song is a live recording. <br> 
  #                + Valence - The higher the value, the more positive mood for the song. <br> 
  #                + Length - The duration of the song.Acousticness - The higher the value the more acoustic the song is. <br> 
  #                + Speechiness - The higher the value the more spoken word the song contains. <br> 
  #                + Duration - The length of the song.')
  #          
  # ),
  
  ## Tab 1 - Welcome -----
  tabPanel(title = "Overview",
           h3("Spotify"),
           p("This is some text introduction"),
           
           # Main panel for page 1
           mainPanel(
             tags$style('.container-fluid {
                             background-color: #E6E6E6;
              }'),
             plotOutput(outputId = "Streamgraph", 
                        brush = brushOpts(id = "plot1_brush", 
                                          direction = "x"
                        )
             ),
             
             plotOutput(outputId = "oneDHeatmap",width = '100%',height = 100),
             
             fluidRow(
               column(
                plotlyOutput(outputId = "Linechart",width = '100%')
                ,width = 8
               ),
               column(
                 plotOutput(outputId = "Radarchart",width = '100%')
                 ,width = 4
               )
             ),
             
             fluidRow(
               column(
                 uiOutput("back2")
                 ,width = 1
               ),
               
               column(4
                      ,sliderInput(inputId = "probs_range", label = "Range of tracks"
                                   ,min = 0.1, max = 1, value = c(0.1, 0.9), step = 0.05
                                   )
                      ),
               column(3
                      ,sliderInput(inputId = "opacity_range", label = "Visibility of top selected tracks"
                                   ,min = 0, max = 1, value = c(0.2, 0.6), step = 0.05
                                   )
                      )
             ),

             plotlyOutput(outputId = "Scatterplot",height = "200%"),
             
             textOutput(outputId = "FilterText"),
             
             numericInput(inputId = "NumArtists",label = "Number of artists:",value =  10, min = 1, max = 50),
             numericInput(inputId = "NumTracks",label = "Number of tracks:",value =  10, min = 1, max = 50),
             
             plotlyOutput("bar"),
             
             uiOutput("back"),
             
             textOutput(outputId = "c_artist"),
             
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
    data <- brushedPoints(df = stream_group_date_artist,brush = input$plot1_brush)
    if (nrow(data) == 0)
      data <- stream_group_date_artist
    
    filter_start_date <<- sort(data$date,decreasing = FALSE)[1] # <<- to assign to global env.
    filter_end_date <<- sort(data$date,decreasing = TRUE)[1] # <<- to assign to global env.
    
    df %>% 
      filter(
        date >= filter_start_date
        ,date <= filter_end_date
        ,artistName %in% current_artist() #in operator, as all artist are set by default
        ,weekday %in% selected_weekday() #in operator, as all weekdays are set by default
        )
    
  })
  
  ### Filter Text ----
  output$FilterText <- renderText({
    masterData() #To activate the master data function
    paste0("You have filtered your data from ",filter_start_date," to ",filter_end_date)
  })
  
  ## Intropage objects ----
  
  output$introText <- renderText({
    paste(total_songs_played(dataInput1 = masterData())
          ,"This corresponds with",total_unique_songs_played(dataInput1 = masterData()),"unique songs"
          ,"across ",total_unique_artist_played(dataInput1 = masterData())," different artists."
          ,"Your favorite artist was:",favourite_artist(dataInput1 = masterData())
          ,"and your favorite song was",favourite_track(dataInput1 = masterData())
          )
  })
  
  
  ## Viz ----
  
  ### Streamgraph ----
  output$Streamgraph <- renderPlot({
    streamgraph2()
  }
  )
  
  ### Line chart ----
  
  output$Linechart <- renderPlotly({
    linechart(dataInput1 = masterData())
  })
  
  ### Scatter plot ----
  output$Scatterplot <- renderPlotly({
    scater_plot_f(dataInput1 = masterData()
                  , probs_range_start = input$probs_range[1]
                  , probs_range_end = input$probs_range[2]
                  , opacity_red = input$opacity_range[2]
                  , opacity_blue = input$opacity_range[1])
  })
  
  ### Radar chart ----
  
  output$Radarchart <- renderPlot({
    radarplot(dataInput1 = masterData())
  })
  
  ### Drill down on line chart ----
  
  selected_weekday <- reactiveVal(value = weekdays) #This function is a container of the selected artist
  output$s_weekday <- renderText({ #text to render selected value
    selected_weekday()
  })
  
  observe({
    pc <- event_data("plotly_click")$x #x = the label, pc = plotly click
    if (isTRUE(pc %in% weekdays)) selected_weekday(pc)
  })
  
  ### Drill down heatmap ----
  
  current_artist <- reactiveVal(value = categories) #This function is a container of the selected artist
  
  output$c_artist <- renderText({
    current_artist()
  })
  
  #### Data for the heatmap ----
  
  heatmap_data <- reactive({
    df_heatmap(dataInput1 = masterData(),selected_weekday = selected_weekday())
  })
  
  ### 1D Heatmap ----
  
  output$oneDHeatmap <- renderPlot({
    oneDHeatmap(heatmap_data = heatmap_data())
  })
  
  #### Data for the bar chart ----
  
  barchart_data <- reactive({
    df_barchart(dataInput1 = masterData(),current_artist = current_artist(),NumArtists = input$NumArtists,NumTracks = input$NumTracks)
  })
  
  output$bar <- renderPlotly({
    barchart(dataInput1 = barchart_data(),current_artist = current_artist())
  })
  
  # update the current category when appropriate
  observe({
    cd <- event_data("plotly_click")$x #x = the label
    if (isTRUE(cd %in% categories)) current_artist(cd)
  })
  
  # populate back button if category is chosen
  output$back <- renderUI({
    if (length(current_artist())) 
      actionButton("clear", "Back", icon("chevron-left"))
  })
  
  output$back2 <- renderUI({
    if (length(selected_weekday())) 
      actionButton(inputId = "clearHeatmap",label = "Back",icon = icon("chevron-left"))
  })
  
  # clear the chosen category on back button press
  observeEvent(eventExpr = input$clear,handlerExpr = current_artist(categories))
  observeEvent(eventExpr = input$clearHeatmap,handlerExpr = selected_weekday(weekdays))
  
  
  
  
  
  ## Test ----
  
  output$BrushedData <- renderDataTable({
    masterData()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)