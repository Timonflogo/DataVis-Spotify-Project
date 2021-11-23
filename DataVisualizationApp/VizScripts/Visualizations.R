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


## Barchart ----

### Barchart data ----

df_barchart <- function(dataInput1,current_artist = current_artist(),selected_weekday = selected_weekday()
                        ,NumArtists = 10,NumTracks = 10){
  
  #Checks if there is selected an artist. If not then:
  if (length(current_artist) != 1) {
    return(
      df_barchart <- dataInput1 %>% 
        filter(weekday %in% selected_weekday) %>% #Filter masterdata with selected weekday 
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
    filter(weekday %in% selected_weekday) %>% #Filter masterdata with selected weekdays 
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
    layout(title = if(length(current_artist) != 1){'Top artists'} else {paste("Top tracks for",current_artist)}) %>%  #go for current Artist unless else is selected
    layout(plot_bgcolor='#E6E6E6') %>% 
    layout(paper_bgcolor='#E6E6E6') %>% 
    layout(xaxis = list(title = if(length(current_artist) != 1){'Artist'} else {'Track'}))
}

### Horizonchart ----

horizonchart <- function(dataInput1, NumArtists) {
  
  target = as.list(df_barchart[1])
  
  horizon_data <- dataInput1 %>% 
    select(time, artistName, msPlayed) %>% 
    filter(artistName %in% unlist(target)) %>% 
    mutate(time = sub(pattern = ":", replacement = "" , time)) %>% 
    mutate(time = sub(pattern = ":00", replacement = "" , time)) %>% 
    mutate(time = as.numeric(time)) %>% 
    mutate(time1 = ifelse(nchar(time) > 3, 
                          yes =  as.numeric(substring(time, first = 1, last = 2)),
                          no = as.numeric(substring(time, first = 1, last = 1)))) %>% 
    mutate(time1 = time1 * 60) %>% 
    mutate(time2 = as.numeric(substring(time, first = 3, last = 4))) %>% 
    mutate(time_minutes = time1 + time2)
  
  horizon_data %>%  
    ggplot() +
    geom_horizon(aes(time_minutes/60, 
                     msPlayed), origin = 'min', horizonscale = 4) +
    facet_wrap(~artistName, ncol = 1, strip.position = 'right') +
    scale_fill_hcl(palette = 'Peach', reverse = T) +
    facet_grid(artistName~.) +
    theme_few() +
    theme(
      plot.background = element_rect(fill = "#E6E6E6", colour = "#E6E6E6"),
      panel.background = element_rect(fill = "#E6E6E6", colour = "#E6E6E6"),
      panel.spacing.y=unit(0, "lines"),
      strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
      legend.position = 'none',
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank(),
      legend.background = element_rect(fill = "#E6E6E6")
    ) +
    scale_x_continuous(
      name = 'Time',
      breaks=seq(from = 3, to = 27, by = 3),
      labels = function(x) {sprintf("%02d:00", as.integer(x %% 24))}) +
    ggtitle('Time of day the Top 10 artists are played')
  
}

