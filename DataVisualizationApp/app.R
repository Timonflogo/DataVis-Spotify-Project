# Libraries
pacman::p_load(
    reshape2,
    ggplot2,
    ggstream,
    streamgraph,
    RColorBrewer,
    Hmisc,
    dplyr,
    gridExtra,
    shiny,
    shinythemes
    ,
    shinyWidgets,
    plotly,
    tidyverse,
    ggalluvial,
    fmsb,
    ragtop
)

# Commands to run on initiating the app
df <- readRDS("Data/stream_selected_c_clean.rds")
source("ConvenienceFunctions/ConvenienceFunctions.R")
source("VizScripts/Visualizations.R")
source("Data/stream_group_data.R")

categories <- unique(df$artist_id)

# UI ----
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    fluid = TRUE,
    titlePanel("Spotify streaming"),
    
    sidebarLayout(
        # Define the sidebar with one input
        sidebarPanel(width = 2,
            helpText(textOutput(outputId = "FilterText"))
        ),
        
        # Main panel for page 1
        mainPanel(plotOutput(
            outputId = "Streamgraph",
            brush = brushOpts(id = "plot1_brush",
                              direction = "x")
        ), width = 10), 
        # column(
        #     width = 12,
        #     style = 'padding:0px;',
        #     plotOutput(
        #         outputId = "Ribbonchart")
        #     )
        
        # plotOutput(outputId = "Ribbonchart"),
        #
        # plotOutput(outputId = "Radarchart"),
        #
        # textOutput(outputId = "FilterText"),
        #
        # numericInput(inputId = "NumArtists",label = "Number of artists:",value =  10, min = 1, max = 50),
        # numericInput(inputId = "NumTracks",label = "Number of tracks:",value =  10, min = 1, max = 50),
        #
        # plotlyOutput("bar"),
        #
        # uiOutput("back"),
        #
        # dataTableOutput(outputId = "BrushedData"),
        #
    )
)




# Server ----
server <- function(input, output) {
    ## Getting data ----
    
    ### masterData ----
    masterData <- reactive({
        df <- readRDS("Data/stream_selected_c_clean.rds")
        data <-
            brushedPoints(df = stream_group_date, brush = input$plot1_brush)
        if (nrow(data) == 0)
            data <- stream_group_date
        
        filter_start_date <<-
            sort(data$date, decreasing = FALSE)[1] # <<- to assign to global env.
        filter_end_date <<-
            sort(data$date, decreasing = TRUE)[1] # <<- to assign to global env.
        
        #Return the data
        if (!length(current_artist())) {
            return(df[date >= filter_start_date & date <= filter_end_date,])
        }
        
        df[date >= filter_start_date &
               date <= filter_end_date &
               artist_id == current_artist(),]
        
    })
    
    ### Filter Text ----
    output$FilterText <- renderText({
        masterData() #To activate the master data function
        paste0("You have filtered your data from: ",
               filter_start_date,
               "<br> to <br>",
               filter_end_date)
    })
    
    ## Viz ----
    
    ### Streamgraph ----
    output$Streamgraph <- renderPlot({
        streamgraph(dataInput1 = stream_group_date)
    })
    
    ### Ribbon chart ----
    
    output$Ribbonchart <- renderPlot({
        ribbonchart(dataInput1 = masterData())
    })
    
    ### Radar chart ----
    
    output$Radarchart <- renderPlot({
        radarplot(dataInput1 = masterData())
    })
    
    
    ### Drill down bar chart ----
    
    current_artist <-
        reactiveVal() #This function is a container of the selected artist
    
    #### Data for the bar chart ----
    
    barplot_data <- reactive({
        if (!length(current_artist())) {
            return(
                df_barchart <- masterData() %>%
                    group_by(artist_id) %>%
                    summarise(msPlayed = sum(msPlayed)) %>%
                    mutate(hsPlayed = msPlayed / 3600000) %>%
                    select(-msPlayed) %>%
                    arrange(desc(hsPlayed)) %>%
                    top_n(n = ifelse(
                        test = is.blank(input$NumArtists),
                        yes = 10,
                        no = input$NumArtists
                    ))
            )
        }
        
        df_barchart <- masterData() %>%
            filter(artist_id %in% current_artist()) %>%
            group_by(track_id) %>%
            summarise(msPlayed = sum(msPlayed)) %>%
            mutate(hsPlayed = msPlayed / 3600000) %>%
            select(-msPlayed) %>%
            arrange(desc(hsPlayed)) %>%
            top_n(n = ifelse(
                test = is.blank(input$NumTracks),
                yes = 10,
                no = input$NumTracks
            ))
    })
    
    output$bar <- renderPlotly({
        d <- setNames(object = barplot_data(), nm = c("x", "y"))
        
        plot_ly(
            data = d
            ,
            x = ~ reorder(x, desc(y)),
            y = ~ y
            ,
            type = "bar"
        ) %>%
            layout(title = current_artist() %||% "Artist") #go for current Artist unless else is selected
        
    })
    
    # update the current category when appropriate
    observe({
        cd <- event_data("plotly_click")$x #x = the label
        if (isTRUE(cd %in% categories))
            current_artist(cd)
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
        barplot_data()
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)