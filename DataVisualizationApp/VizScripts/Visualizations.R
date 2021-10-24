## Streamgraph ----
streamgraph <- function(dataInput1){
  #source("ConvenienceFunctions/ConvenienceFunctions.R")
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



## Ribbon chart ---- 
ribbonchart <- function(dataInput1){
  stream_group_weekday <- dataInput1 %>% 
    melt(1:13) %>% # melt(which(names(dataInput1) == "track_id"):which(names(dataInput1) == "duration_ms")) %>% # dont use this, its not working
    group_by(weekday, variable) %>% 
    summarise(value = sum(value)) %>% 
    mutate(weekday = ordered(weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                               "Friday", "Saturday", "Sunday")))
  
  ggplot(data = stream_group_weekday,
         aes(x = weekday
             , y = value
             , alluvium = variable)) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = 9,name = 'Blues')) +
    geom_alluvium(aes(fill = cap_space(string = variable))
                  , alpha = .75
                  , decreasing = FALSE
                  , linetype = 0) +
    theme(panel.background = element_blank()
          , panel.grid.major.x = element_line(size = 0.3
                                              , color="black"
                                              ,linetype = "dotted")
          , axis.ticks = element_blank()) +
    labs(fill = 'Features',x = "Time")
}


## Radar chart ----

radarplot <- function(dataInput1){
  
  radar_gg <- dataInput1 %>%
    melt(1:13) %>%  # melt(which(names(dataInput1) == "track_id"):which(names(dataInput1) == "duration_ms")) %>% dont use this, its not working
    group_by(variable) %>%
    summarise(value = mean(value)) %>% 
    spread(variable, value)
  
  rownames(radar_gg) <- "exposure"
  radar_gg <- rbind(rep(1,ncol(radar_gg)) , rep(0,ncol(radar_gg)) , radar_gg)
  
  colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
  colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
  
  # plot with default options:
  radarchart( radar_gg, axistype=1 ,vlabels = cap_space(colnames(radar_gg)),
              #custom polygon
              pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
              #custom the grid
              cglcol = "grey", cglty=1, axislabcol= "grey",caxislabels=seq(0,1,0.25), cglwd=0.8, #
              #custom labels
              vlcex=0.8
  )
}

## Barchart ----

### Barchart data ----

df_barchart <- function(dataInput1,current_artist = current_artist()
                        ,NumArtists = 10,NumTracks = 10){
  
  #Checks if there is selected an artist. If not then:
  if (!length(current_artist)) {
    return(
      df_barchart <- dataInput1 %>% 
        group_by(artist_id,artistName) %>% 
        summarise(msPlayed = sum(msPlayed)) %>%
        mutate(hsPlayed = msPlayed/3600000) %>%
        ungroup() %>% 
        select(c(artistName,hsPlayed)) %>% 
        arrange(desc(hsPlayed)) %>% 
        top_n(n = ifelse(test = is.blank(NumArtists),yes = 10,no = NumArtists))
    )
  }
  
  #Else the following is run:
  df_barchart <- dataInput1 %>% 
    filter(artistName %in% current_artist) %>% 
    group_by(track_id,trackName) %>% 
    summarise(msPlayed = sum(msPlayed)) %>% 
    mutate(hsPlayed = msPlayed/3600000) %>% 
    ungroup() %>%
    select(c(trackName,hsPlayed)) %>% 
    arrange(desc(hsPlayed)) %>% 
    top_n(n = ifelse(test = is.blank(NumTracks),yes = 10,no = NumTracks))
  
  #Return the data frame which is used for the plot
  return(df_barchart)
}

### Barchart plot ----

barchart <- function(dataInput1,current_artist = current_artist()){
  
  #Creating a dataframe where the variables are fixed to be named x and y
  d <- as.data.frame(dataInput1) %>% setNames(nm = c("x", "y"))
  
  #Plotting the barchart
  plot_ly(data = d
          ,x = ~reorder(x,desc(y)), y = ~y
          ,type = "bar") %>%
    layout(title = current_artist %||% "Artist") #go for current Artist unless else is selected
}