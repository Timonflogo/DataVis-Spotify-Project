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
    theme(plot.background = element_rect(fill = "#E6E6E6", colour = "#E6E6E6")
          ,panel.background = element_rect(fill = "#E6E6E6", colour = "#E6E6E6") 
          ,panel.grid.major.x = element_line(size = 0.3, color="black",linetype = "dotted")
          ,panel.grid.minor = element_blank() #Remove gridlines
          ,panel.grid.major.y = element_blank() #Remove gridlines
          ,legend.background = element_rect(fill = "#E6E6E6")
          ,axis.ticks = element_blank()) +
    labs(fill = 'Features',x = "Time")
}

### Linechart ----

linechart <- function(dataInput1){
  line_group_weekday <- dataInput1 %>% 
    melt(1:13) %>% # melt(which(names(dataInput1) == "track_id"):which(names(dataInput1) == "duration_ms")) %>% # dont use this, its not working
    group_by(weekday, variable) %>% 
    summarise(value = sum(value)) %>% 
    mutate(weekday = ordered(weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                               "Friday", "Saturday", "Sunday"))) %>% 
    spread(variable, value)
  
  
  Linechart_plotly <- plot_ly(line_group_weekday, x = ~weekday, y = ~danceability_exposed , name = 'Danceability', type = 'scatter', mode = 'marker',
                              line = list(color = 'rgb(166,206,227)', width = 4))
  Linechart_plotly <- Linechart_plotly %>% add_trace(y = ~energy_exposed, name = 'Energy', line = list(color = 'rgb(31,120,180)', width = 4))
  Linechart_plotly <- Linechart_plotly %>% add_trace(y = ~loudness_exposed, name = 'Loudness', line = list(color = 'rgb(178,223,138)', width = 4))
  Linechart_plotly <- Linechart_plotly %>% add_trace(y = ~speechiness_exposed, name = 'Speechiness', line = list(color = 'rgb(51,160,44)', width = 4))
  Linechart_plotly <- Linechart_plotly %>% add_trace(y = ~acousticness_exposed, name = 'Acousticness', line = list(color = 'rgb(251,154,153)', width = 4))
  Linechart_plotly <- Linechart_plotly %>% add_trace(y = ~instrumentalness_exposed, name = 'Instrumentalness', line = list(color = 'rgb(227,26,28)', width = 4))
  Linechart_plotly <- Linechart_plotly %>% add_trace(y = ~liveness_exposed, name = 'Liveness', line = list(color = 'rgb(253,191,111)', width = 4))
  Linechart_plotly <- Linechart_plotly %>% add_trace(y = ~valence_exposed, name = 'Valence', line = list(color = 'rgb(255,127,0)', width = 4))
  Linechart_plotly <- Linechart_plotly %>% add_trace(y = ~tempo_exposed, name = 'Tempo', line = list(color = 'rgb(202,178,214)', width = 4))
  Linechart_plotly <- Linechart_plotly %>% layout(title = "Aggregated weekly Music exposure split up by audio features",
                                                  xaxis = list(title = "Weekday"),
                                                  yaxis = list (title = "Music Exposure"),
                                                  plot_bgcolor  = "rgb(230, 230, 230)", 
                                                  paper_bgcolor = "rgb(230, 230, 230)",
                                                  fig_bgcolor   = "rgb(230, 230, 230)")
  Linechart_plotly
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
  par(bg = '#E6E6E6')
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
    layout(title = current_artist %||% "Artist") %>%  #go for current Artist unless else is selected
    layout(plot_bgcolor='#E6E6E6') %>% 
    layout(paper_bgcolor='#E6E6E6')
}