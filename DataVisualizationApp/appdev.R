# Libraries
pacman::p_load(reshape2,ggplot2,ggstream,streamgraph,RColorBrewer,Hmisc,dplyr,gridExtra,shiny,shinythemes
               ,shinyWidgets,plotly,tidyverse,ggalluvial,fmsb,ragtop)

# Commands to run on initiating the app
df <- readRDS("Data/stream_selected_c_clean.rds")
source("ConvenienceFunctions/ConvenienceFunctions.R")
source("VizScripts/Visualizations.R")
source("VizScripts/Streamgraph.R")
source("VizScripts/Summary_stats.R")
source("Data/stream_group_data.R")

categories <- unique(df$artistName)

# UI ----
ui <- navbarPage(
  theme = shinytheme("cerulean"),
  fluid = TRUE, 
  "Spotify streaming",
  
  tabPanel(title = "Welcome",
           h1("Introduction"),
           
           textOutput(outputId = "introText"),

           p(),
           
           HTML('+ Energy - The energy of a song - the higher the value, the more energtic song <br> 
                 + Danceability - The higher the value, the easier it is to dance to this song. <br> 
                 + Loudness (dB) - The higher the value, the louder the song. <br> 
                 + Liveness - The higher the value, the more likely the song is a live recording. <br> 
                 + Valence - The higher the value, the more positive mood for the song. <br> 
                 + Length - The duration of the song.Acousticness - The higher the value the more acoustic the song is. <br> 
                 + Speechiness - The higher the value the more spoken word the song contains. <br> 
                 + Duration - The length of the song.')
           
  ),
  
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
    data <- brushedPoints(df = stream_group_date,brush = input$plot1_brush)
    if (nrow(data) == 0)
      data <- stream_group_date
    
    filter_start_date <<- sort(data$date,decreasing = FALSE)[1] # <<- to assign to global env.
    filter_end_date <<- sort(data$date,decreasing = TRUE)[1] # <<- to assign to global env.
    
    #Return the data
    if (!length(current_artist())) {
      return(
        df[date >= filter_start_date & date <= filter_end_date,]
      )
    }
    
    df[date >= filter_start_date & date <= filter_end_date & artistName == current_artist(),]
    
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
  
  ### Ribbon chart ----
  
  output$Ribbonchart <- renderPlot({
    ribbonchart(dataInput1 = masterData())
  })
  
  ### Radar chart ----
  
  output$Radarchart <- renderPlot({
    radarplot(dataInput1 = masterData())
  })
  
  
  ### Drill down bar chart ----
  
  current_artist <- reactiveVal() #This function is a container of the selected artist
  
  output$c_artist <- renderText({
    current_artist()
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
  
  # clear the chosen category on back button press
  observeEvent(input$clear, current_artist(NULL))
  
  
  
  
  
  ## Test ----
  
  output$BrushedData <- renderDataTable({
    barchart_data()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)