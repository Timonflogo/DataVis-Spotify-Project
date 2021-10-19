# Libraries
# require("remotes")
# remotes::install_github("hrbrmstr/streamgraph")
pacman::p_load(reshape2,ggplot2,ggstream,streamgraph,RColorBrewer,Hmisc,dplyr,gridExtra,shiny, shinythemes, shinyWidgets, plotly, tidyverse,ggalluvial)

# Commands to run on initiating the app
df <- readRDS("Data/stream_selected_c_clean.rds") #Shortened, if run manually instert: DataVisualizationApp/
source("ConvenienceFunctions/ConvenienceFunctions.R")
source("VizScripts/Visualizations.R")

#Load aggregated features
stream_group_date <- df %>% 
    melt(1:11) %>% 
    group_by(date, month, variable) %>% 
    summarise(value = sum(value))
stream_group_date$index <- sort(rep(seq(1,nrow(stream_group_date)/length(unique(stream_group_date$variable))),9)) #Create an index to map the y values onto a continous x axis


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
                 
                 textOutput(outputId = "FilterText"),
                 
                 dataTableOutput(outputId = "BrushedData"),
                 dataTableOutput(outputId = "BrushedData1"),
                 
                 width = 11
             )
    )
)

# Server ----
server <- function(input,output){
    
    ## Getting data ----
    
    ### masterData ----
    masterData <- reactive({
        
        df <- readRDS("Data/stream_selected_c_clean.rds") #Shortened, if run manually instert: DataVisualizationApp/
        data <- brushedPoints(df = stream_group_date,brush = input$plot1_brush)
        if (nrow(data) == 0)
            data <- stream_group_date
        
        filter_start_date <<- sort(data$date,decreasing = FALSE)[1] # <<- to assign to global env.
        filter_end_date <<- sort(data$date,decreasing = TRUE)[1] # <<- to assign to global env.
        
        #Return the data
        df[date >= filter_start_date & date <= filter_end_date,]
        
    })
    
    ### stream_weekday ----
    stream_weekday <- reactive({
        
        stream_group_weekday <- masterData() %>% 
            melt(1:11) %>% 
            filter(date >= filter_start_date & date <= filter_end_date) %>% 
            group_by(weekday, variable) %>% 
            summarise(value = sum(value))
        
        #Return the data
        stream_group_weekday
    })
    
    
    ### Filter Text ----
    output$FilterText <- renderText({
        
        masterData() #To activate the master data function
        
        paste0("You have filtered your data from ",filter_start_date," to ",filter_end_date)
    })
    
    
    ## Viz ----
    
    ### Streamgraph ----
    output$Streamgraph <- renderPlot({
        
        idx <- seq(from = 1,to = length(stream_group_date$index),by = 9 * (367 / 12)) #months are on average 30.5 days
        
        ggplot(stream_group_date, aes(x = index, y = value, fill = cap_space(string = variable))) +
            geom_stream(n_grid = 375,bw = 0.50) + #bw = wigglyness
            scale_fill_manual(values = RColorBrewer::brewer.pal(n = 9,name = 'Blues')) +
            theme(panel.background = element_blank()
                  ,panel.grid.major.x = element_line(size = 0.3, color="black",linetype = "dotted")
                  ,axis.text.x = element_text(angle = 0)
                  ,axis.ticks = element_blank()
            ) +
            scale_x_continuous(labels = stream_group_date$month[idx]
                               ,breaks = stream_group_date$index[idx]
                               ,n.breaks = length(stream_group_date$month[idx])) +
            labs(fill = 'Features',x = "Time")
    }
    )
    
    ### Ribbon chart ----
    
    output$Ribbonchart <- renderPlot({
        
        ribbonchart(dataInput1 = masterData())
        
    })
    
    ## Test ----
    
    output$BrushedData <- renderDataTable({
        masterData()
    })
    
    output$BrushedData1 <- renderDataTable({
        stream_weekday()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)