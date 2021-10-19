# Libraries
# require("remotes")
# remotes::install_github("hrbrmstr/streamgraph")
pacman::p_load(reshape2,ggplot2,ggstream,plotly,streamgraph,RColorBrewer,Hmisc,dplyr,gridExtra,shiny, shinythemes, shinyWidgets, plotly, tidyverse)

# Commands to run on initiating the app
df <- readRDS("Data/stream_selected_c_clean.rds") #Shortened, if run manually instert: DataVisualizationApp/
source("ConvenienceFunctions/ConvenienceFunctions.R")
source("VizScripts/Visualizations.R")

#Load aggregated features
stream_gg <- df %>% 
    melt(1:11) %>%  #Keep columns 1 - 9, create a row entry for each column value of 10 - 18
    group_by(year_week,month,variable) %>% 
    summarise(value = sum(value))

stream_group_date <- df %>% 
    melt(1:11) %>% 
    group_by(date, month, variable) %>% 
    summarise(value = sum(value))

stream_group_weekday <- df %>% 
    melt(1:11) %>% 
    filter(date >= "2020-09-23" & date <= "2020-10-07") %>% 
    group_by(weekday, variable) %>% 
    summarise(value = sum(value))

#Create an index to map the y values onto a continous x axis
stream_group_date$index <- sort(rep(seq(1,nrow(stream_group_date)/length(unique(stream_group_date$variable))),9))


streamgraph(dataInput1 = df)

ui <- navbarPage(
    theme = shinytheme("cerulean"),
    fluid = TRUE, 
    "Movie recommender system",
    
    #' *Tab 1*
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
                 plotOutput(outputId = "Streamgraph1"
                            ),
                 
                 textOutput(outputId = "Text"),
                 
                 dataTableOutput(outputId = "BrushedData"
                             ),
                 
                 width = 11)
                 
                 ),
    
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
    
    selectedData <- reactive({
        data <- brushedPoints(stream_group_date, input$plot1_brush)
        if (nrow(data) == 0)
            data <- stream_group_date
        
        #Return the data
        data
    })
    
    output$Text <- renderText({
        
        data <- brushedPoints(stream_group_date, input$plot1_brush)
        if (nrow(data) == 0)
            data <- stream_group_date

        #Filter values
        filter_start_date <<- sort(data$date,decreasing = FALSE)[1]
        filter_end_date <<- sort(data$date,decreasing = TRUE)[1]
                
        paste0("You have filtered your data from ",filter_start_date," to ",filter_end_date)
    })
    
    stream_weekday <- reactive({
        stream_group_weekday <- df %>% 
            melt(1:11) %>% 
            filter(date >= filter_start_date & date <= filter_end_date) %>% 
            group_by(weekday, variable) %>% 
            summarise(value = sum(value))
        
        #Return the data
        stream_group_weekday
    })
    
    
    output$Streamgraph<-renderPlot({
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
    
    output$Streamgraph1<-renderPlot({
        # idx <- seq(from = 1,to = length(stream_gg$index),by = 9 * 4.45) #months are on avg. 4.5 weeks
        
        ggplot(stream_weekday(), aes(x = weekday, y = value, fill = cap_space(string = variable))) +
            geom_stream(n_grid = 57,bw = 0.5) + #bw = wigglyness
            scale_fill_manual(values = RColorBrewer::brewer.pal(n = 9,name = 'Blues')) +
            theme(panel.background = element_blank()
                  ,panel.grid.major.x = element_line(size = 0.3, color="black",linetype = "dotted")
                  ,axis.text.x = element_text(angle = 0)
                  ,axis.ticks = element_blank()
            ) +
            # scale_x_continuous(labels = stream_gg$month[idx]
            #                    ,breaks = stream_gg$index[idx]
            #                    ,n.breaks = length(stream_gg$month[idx])) +
            labs(fill = 'Features',x = "Time")
        }
    )
    
    output$BrushedData <- renderDataTable({
        stream_weekday()
    })
}



# Run the application 
shinyApp(ui = ui, server = server)