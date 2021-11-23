#df <- readRDS("Data/stream_selected_c_clean.rds") 
#dataInput1 <- df

## Function for extracting seasons
getSeason <- function(DATES) {
  WS <- as.Date("2021-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2021-3-20",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2021-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2021-9-22",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2021-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Autmn")))
}

# Radarchart function

radarplot <- function(dataInput1,selected_weekday = selected_weekday()){
  
  radar_gg <- dataInput1 %>%
    filter(weekday %in% selected_weekday) %>% #Filter masterdata with selected weekday
    mutate(season = as.factor(getSeason(date))) %>%
    select(1, 2:6, 42, 7:41) %>% 
    melt(1:14) %>%  # melt(which(names(dataInput1) == "track_id"):which(names(dataInput1) == "duration_ms")) %>% dont use this, its not working
    group_by(season, variable) %>%
    summarise(value = mean(value)) %>% 
    spread(variable, value) %>% 
    select(21:28) %>% 
    mutate(season = as.character(season))
  
  radar_gg <- radar_gg %>%
    remove_rownames %>%
    column_to_rownames(var="season") %>%
    as.data.frame() %>% 
    round(digits = 2)
  

  
  fig <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  ) 
  fig <- fig %>%
    add_trace(
      r = as.numeric(radar_gg[1,]),
      theta = cap_space_radar(colnames(radar_gg)),
      name = 'Autmn'
    ) 
  fig <- fig %>%
    add_trace(
      r = as.numeric(radar_gg[4,]),
      theta = cap_space_radar(colnames(radar_gg)),
      name = 'Winter'
    ) 
  fig <- fig %>%
    add_trace(
      r = as.numeric(radar_gg[2,]),
      theta = cap_space_radar(colnames(radar_gg)),
      name = 'Spring'
    ) 
  fig <- fig %>%
    add_trace(
      r = as.numeric(radar_gg[3,]),
      theta = cap_space_radar(colnames(radar_gg)),
      name = 'Summer'
    ) 
  fig <- fig %>%
    layout(
      title = "Average Music exposure",
      font = 1, 
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,1)
          )),
      plot_bgcolor  = "rgb(230, 230, 230)", 
      paper_bgcolor = "rgb(230, 230, 230)",
      fig_bgcolor   = "rgb(230, 230, 230)",
      autosize = F, height = 500, margin = list(l=50, r=50, b=50, t=50, pad=1)
    )
  

  
  fig
  
  # radar_gg <- rbind(rep(1,ncol(radar_gg)) , rep(0,ncol(radar_gg)) , radar_gg)
  # 
  # colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.3,0.6,0.1,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.8,0.2,0.5,0.9) )
  # colors_in=c( rgb(0.2,0.5,0.3,0.1), rgb(0.3,0.6,0.1,0.1), rgb(0.7,0.5,0.3,0.1), rgb(0.8,0.2,0.5,0.1) )
  # 
  # # plot with default options:
  # par(bg = '#E6E6E6', mar = c(1,1,0.1, 0.1) )
  # radarchart( radar_gg, axistype=1,vlabels = cap_space_radar(colnames(radar_gg)),
  #             #custom polygon
  #             pcol=colors_border , pfcol=colors_in , plwd=2 , plty=1,
  #             #custom the grid
  #             cglcol = "grey", cglty=1, axislabcol= "grey",caxislabels=seq(0,1,0.25), cglwd=0.8, #
  #             #custom labels
  #             vlcex=0.8,
  #             # change opacity
  #             
  # )
  # 
  # legend(x=.85, y=1.3, legend = rownames(radar_gg[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=1.2, pt.cex=3)
}