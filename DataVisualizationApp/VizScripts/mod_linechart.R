#df <- readRDS("Data/stream_selected_c_clean.rds") 
#dataInput1 <- df

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
