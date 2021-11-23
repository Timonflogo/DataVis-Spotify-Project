
source("Data/stream_group_data.R")
source("ConvenienceFunctions/ConvenienceFunctions.R")

# THIS HAS BEEN DEPRECIATED 
# ## Streamgraph ----
# streamgraph <- function(dataInput1){
#   #source("ConvenienceFunctions/ConvenienceFunctions.R")
#   idx <- seq(from = 1,to = length(stream_group_date$index),by = 9 * (367 / 12)) #months are on average 30.5 days
#   
#   ggplot(stream_group_date, aes(x = index, y = value, fill = cap_space(string = variable))) +
#     geom_stream(n_grid = 375,bw = 0.50) + #bw = wigglyness
#     scale_fill_manual(values = RColorBrewer::brewer.pal(n = 9,name = 'Blues')) +
#     theme(plot.background = element_rect(fill = "#E6E6E6", colour = "#E6E6E6")
#           ,panel.background = element_rect(fill = "#E6E6E6", colour = "#E6E6E6") 
#           ,panel.grid.major.x = element_line(size = 0.3, color="black",linetype = "dotted")
#           ,panel.grid.minor = element_blank() #Remove gridlines
#           ,panel.grid.major.y = element_blank() #Remove gridlines
#           ,axis.text.x = element_text(angle = 0)
#           ,axis.ticks = element_blank()
#           ,legend.background = element_rect(fill = "#E6E6E6")
#     ) +
#     scale_x_continuous(labels = stream_group_date$month[idx]
#                        ,breaks = stream_group_date$index[idx]
#                        ,n.breaks = length(stream_group_date$month[idx])) +
#     labs(fill = 'Features',x = "Time")
# }

## Streamgraph2 ----
streamgraph2 <- function(){
  
  xax <- stream_group_date_artist %>% 
    ungroup() %>% 
    select(date) %>%
    mutate(year = as.numeric(substr(x = date,start = 1,stop = 4))
           ,monthNo = as.numeric(substr(x = date,start = 6,stop = 7))) %>% 
    mutate(monthName = month.name[monthNo]) %>% 
    distinct() %>% 
    group_by(year,monthNo) %>% 
    mutate(rn = 1:n()) %>% 
    ungroup() %>% 
    mutate(rn2 = 1:n()) %>% 
    filter(rn == 1)

  ggplot(stream_group_date_artist, aes(x = index, y = hsPlayed, fill = category,label = category)) +
    geom_stream() + #bw = wigglyness
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = 12,name = 'Set3')) +
    theme(plot.background = element_rect(fill = "#E6E6E6", colour = "#E6E6E6")
          ,panel.background = element_rect(fill = "#E6E6E6", colour = "#E6E6E6") 
          ,panel.grid.major.x = element_line(size = 0.3, color="black",linetype = "dotted")
          ,panel.grid.minor = element_blank() #Remove gridlines
          ,panel.grid.major.y = element_blank() #Remove gridlines
          ,axis.text.x = element_text(angle = 0)
          ,axis.ticks = element_blank()
          ,legend.background = element_rect(fill = "#E6E6E6")
          ,plot.title = element_text(hjust = 0.5,size = 16,color = "black",face = 'bold')
    ) +
    scale_x_continuous(labels = xax$monthName #Vector of labels
                       ,breaks = xax$rn2
                       ) +
    labs(fill = 'Features',x = "Time",y = "Hours played") +
    ggtitle("Amount of streaming")
}

#streamgraph2()

