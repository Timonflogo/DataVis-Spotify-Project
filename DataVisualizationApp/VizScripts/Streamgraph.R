## Streamgraph ----
streamgraph <- function(dataInput1){
  #source("ConvenienceFunctions/ConvenienceFunctions.R")
  idx <- seq(from = 1,to = length(stream_group_date$index),by = 9 * (367 / 12)) #months are on average 30.5 days
  
  ggplot(stream_group_date, aes(x = index, y = value, fill = cap_space(string = variable))) +
    geom_stream(n_grid = 375,bw = 0.50) + #bw = wigglyness
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = 9,name = 'Blues')) +
    theme(plot.background = element_rect(fill = "#E6E6E6", colour = "#E6E6E6")
          ,panel.background = element_rect(fill = "#E6E6E6", colour = "#E6E6E6") 
          ,panel.grid.major.x = element_line(size = 0.3, color="black",linetype = "dotted")
          ,panel.grid.minor = element_blank() #Remove gridlines
          ,panel.grid.major.y = element_blank() #Remove gridlines
          ,axis.text.x = element_text(angle = 0)
          ,axis.ticks = element_blank()
          ,legend.background = element_rect(fill = "#E6E6E6")
    ) +
    scale_x_continuous(labels = stream_group_date$month[idx]
                       ,breaks = stream_group_date$index[idx]
                       ,n.breaks = length(stream_group_date$month[idx])) +
    labs(fill = 'Features',x = "Time")
}

