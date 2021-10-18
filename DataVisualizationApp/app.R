# Libraries
# require("remotes")
# remotes::install_github("hrbrmstr/streamgraph")
pacman::p_load(reshape2,ggplot2,ggstream,plotly,streamgraph,RColorBrewer,Hmisc,dplyr,gridExtra)
pacman::p_load(shiny, shinythemes, shinyWidgets, plotly, tidyverse)


# Define a Streamgraph displaying Spotify audio features over time 
# Load Data ----

df <- readRDS("R dataframe/stream_selected_c_clean.rds")
print(df)
df

stream_gg <- df %>% 
    melt(1:11) %>%  #Keep columns 1 - 9, create a row entry for each column value of 10 - 18
    group_by(year_week,month,variable) %>% 
    summarise(value = sum(value))

#Create an index to map the y values onto a continous x axis
stream_gg$index <- sort(rep(seq(1,55),9))


ui <- navbarPage(
    theme = shinytheme("cerulean"),
    fluid = TRUE, 
    "Movie recommender system",
    
    #' *Tab 1*
    tabPanel(title = "Welcome",
             h3("Spotify of making movie recommendations"),
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
        data <- brushedPoints(stream_gg, input$plot1_brush)
        if (nrow(data) == 0)
            data <- stream_gg
        data
    })
    
    
    output$Streamgraph<-renderPlot({
        cap_space <- function(string){capitalize(str_replace(string,pattern = "_",replacement = " "))}
        idx <- seq(from = 1,to = length(stream_gg$index),by = 9 * 4.45) #months are on avg. 4.5 weeks
        
        ggplot(stream_gg, aes(x = index, y = value, fill = cap_space(string = variable))) +
            geom_stream(n_grid = 57,bw = 0.25) + #bw = wigglyness
            #geom_stream_label(aes(label = cap_space(string = variable))) + 
            scale_fill_manual(values = RColorBrewer::brewer.pal(n = 9,name = 'Blues')) +
            theme(panel.background = element_blank()
                  ,panel.grid.major.x = element_line(size = 0.3, color="black",linetype = "dotted")
                  ,axis.text.x = element_text(angle = 0)
                  ,axis.ticks = element_blank()
            ) +
            scale_x_continuous(labels = stream_gg$month[idx]
                               ,breaks = stream_gg$index[idx]
                               ,n.breaks = length(stream_gg$month[idx])) +
            labs(fill = 'Features',x = "Time")
        }
    )
    
    output$Streamgraph1<-renderPlot({
        cap_space <- function(string){capitalize(str_replace(string,pattern = "_",replacement = " "))}
        idx <- seq(from = 1,to = length(stream_gg$index),by = 9 * 4.45) #months are on avg. 4.5 weeks
        
        ggplot(selectedData(), aes(x = index, y = value, fill = cap_space(string = variable))) +
            geom_stream(n_grid = 57,bw = 0.5) + #bw = wigglyness
            #geom_stream_label(aes(label = cap_space(string = variable))) + 
            scale_fill_manual(values = RColorBrewer::brewer.pal(n = 9,name = 'Blues')) +
            theme(panel.background = element_blank()
                  ,panel.grid.major.x = element_line(size = 0.3, color="black",linetype = "dotted")
                  ,axis.text.x = element_text(angle = 0)
                  ,axis.ticks = element_blank()
            ) +
            scale_x_continuous(labels = stream_gg$month[idx]
                               ,breaks = stream_gg$index[idx]
                               ,n.breaks = length(stream_gg$month[idx])) +
            labs(fill = 'Features',x = "Time")
        }
    )
}



# Run the application 
shinyApp(ui = ui, server = server)