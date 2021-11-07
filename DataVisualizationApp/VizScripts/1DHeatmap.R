

#df <- readRDS("Data/stream_selected_c_clean.rds") 

oneDHeatmap <- function(dataInput1){
  
  options(scipen = 999)
  
  totaltime <- expand.grid(c(0:23),c(0:59))
  totaltime$Var1 <- ifelse(nchar(totaltime$Var1) == 1,paste0('0',totaltime$Var1),no = totaltime$Var1)
  totaltime$Var2 <- ifelse(nchar(totaltime$Var2) == 1,paste0('0',totaltime$Var2),no = totaltime$Var2)
  totaltime <- plyr::mdply(totaltime, 'paste', sep = ':')
  totaltime <- totaltime[order(totaltime$Var1,totaltime$Var2),]
  totaltime$index <- seq(from = 0,to = 1439,by = 1)
  totaltime <- totaltime %>% 
    dplyr::rename(time = V1) %>% 
    select(index,time)
  
  d <- dataInput1 %>% 
    select(time,msPlayed) %>% 
    group_by(time) %>% 
    mutate(msPlayed = sum(msPlayed)
           ,mPlayed = msPlayed / 60000 #msPlayed is already aggregated
           ,time = substr(time,start = 1,stop = 5)) %>%
    unique()
  
  data <- totaltime %>% 
    left_join(d,by = 'time')
  
  xax <- totaltime %>% #THE MASTERDATA
    mutate(hour = substr(time,1,2)
           ,rn2 = 1:n()) %>% 
    group_by(hour) %>% 
    mutate(rn = 1:n()) %>% 
    filter(rn == 1)
  
  ggplot(data, aes(x = index,y = 1, fill = mPlayed)) + 
    geom_tile() +
    #scale_fill_manual(values = RColorBrewer::brewer.pal(n = 12,name = 'Set3')) +
    theme(plot.background = element_rect(fill = "#E6E6E6", colour = "#E6E6E6")
          ,panel.background = element_rect(fill = "#E6E6E6", colour = "#E6E6E6") 
          ,panel.grid.major.x = element_line(size = 0.3, color="black",linetype = "dotted")
          ,panel.grid.minor = element_blank() #Remove gridlines
          ,panel.grid.major.y = element_blank() #Remove gridlines
          ,axis.text.x = element_text(angle = 0)
          ,axis.ticks = element_blank()
          ,legend.background = element_rect(fill = "#E6E6E6")
    ) +
    scale_x_continuous(labels = xax$hour #Vector of labels
                       ,breaks = xax$rn2
    ) +
    labs(fill = 'Time played',x = "Hour",y = "Hours played")
}
