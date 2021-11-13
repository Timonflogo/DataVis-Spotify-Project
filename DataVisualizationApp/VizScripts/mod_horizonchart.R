#df <- readRDS("Data/stream_selected_c_clean.rds") 
#dataInput1 <- df

# Horizonchart function

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