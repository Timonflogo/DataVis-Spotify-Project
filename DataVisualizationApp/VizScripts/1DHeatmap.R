

#df <- readRDS("Data/stream_selected_c_clean.rds") 
#dataInput1 <- df

df_heatmap <- function(dataInput1,selected_weekday = selected_weekday()) {
  
  #Checks if there is selected an artist. If not then:
  if (!length(selected_weekday)) {
    return(
      df_heatmap <- dataInput1 %>% #The masterdata
        select(time,msPlayed) %>% 
        group_by(time) %>% 
        mutate(msPlayed = sum(msPlayed)
               ,mPlayed = msPlayed / 60000 #msPlayed is already aggregated
               ,time = substr(time,start = 1,stop = 5)) %>%
        unique()
    )
  }
  
  #Else the following is run:
  df_heatmap <- dataInput1 %>% #The masterdata
    filter(weekday %in% selected_weekday) %>% #Filter masterdata with selected weekday
    select(time,msPlayed) %>% 
    group_by(time) %>% 
    mutate(msPlayed = sum(msPlayed)
           ,mPlayed = msPlayed / 60000 #msPlayed is already aggregated
           ,time = substr(time,start = 1,stop = 5)) %>%
    unique()
  
  #Return the data frame which is used for the plot
  return(df_heatmap)
}

oneDHeatmap <- function(heatmap_data = heatmap_data()){
  
  options(scipen = 999)
  
  ## d = df_heatmap!!! This is moved to the data reactive function
  
  # d <- dataInput1 %>% 
  #   select(time,msPlayed) %>% 
  #   group_by(time) %>% 
  #   mutate(msPlayed = sum(msPlayed)
  #          ,mPlayed = msPlayed / 60000 #msPlayed is already aggregated
  #          ,time = substr(time,start = 1,stop = 5)) %>%
  #   unique()

  #Create a grid of all hour:minute value during a day
  totaltime <- expand.grid(c(0:23),c(0:59))
  totaltime$Var1 <- ifelse(nchar(totaltime$Var1) == 1,paste0('0',totaltime$Var1),no = totaltime$Var1)
  totaltime$Var2 <- ifelse(nchar(totaltime$Var2) == 1,paste0('0',totaltime$Var2),no = totaltime$Var2)
  totaltime <- plyr::mdply(totaltime, 'paste', sep = ':')
  totaltime <- totaltime[order(totaltime$Var1,totaltime$Var2),]
  totaltime$index <- seq(from = 0,to = 1439,by = 1)
  totaltime <- totaltime %>% 
    dplyr::rename(time = V1) %>% 
    select(index,time)
  
  #Create the dataframe for the plot. Aggregation of the data pr. hour:minute
  data <- totaltime %>% #We use total time, to preserve empty minutes as empty
    left_join(heatmap_data,by = 'time')
  
  #Creating x-axis labels to show the hour time stamps
  xax <- totaltime %>%
    mutate(hour = substr(time,1,2)
           ,rn2 = 1:n()) %>% 
    group_by(hour) %>% 
    mutate(rn = 1:n()) %>% 
    filter(rn == 1)
  
  #The plot
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
          ,axis.ticks.y = element_blank()
          ,axis.text.y = element_blank()
          ,legend.background = element_rect(fill = "#E6E6E6")
    ) +
    scale_x_continuous(labels = xax$hour #Vector of labels
                       ,breaks = xax$rn2
    ) +
    labs(fill = 'Time played',x = "Hour",y = "Hours played")
}
