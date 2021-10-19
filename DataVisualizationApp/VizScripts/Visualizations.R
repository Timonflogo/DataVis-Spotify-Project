## Streamgraph ----
streamgraph <- function(dataInput1){
source("ConvenienceFunctions/ConvenienceFunctions.R")

#Load aggregated features
stream_gg <- dataInput1 %>% 
  melt(1:11) %>%  #Keep columns 1 - 9, create a row entry for each column value of 10 - 18
  group_by(year_week,month,variable) %>% 
  summarise(value = sum(value))
stream_gg$index <- sort(rep(seq(1,55),9))

idx <- seq(from = 1,to = length(stream_gg$index),by = 9 * 4.45) #months are on avg. 4.5 weeks

  ggplot(stream_gg, aes(x = index, y = value, fill = cap_space(string = variable))) +
    geom_stream(n_grid = 57,bw = 0.5) + #bw = wigglyness
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = 9,name = 'Blues')) +
    theme(panel.background = element_blank()
          ,panel.grid.major.x = element_line(size = 0.3, color="black",linetype = "dotted")
          ,axis.text.x = element_text(angle = 0)
          ,axis.ticks = element_blank()
    ) +
    scale_x_continuous(labels = stream_gg$month[idx]
                       ,breaks = stream_gg$index[idx]
                       ,n.breaks = length(stream_gg$month[idx])) +
    labs(fill = 'Features',x = "Time")
}

#streamgraph(dataInput = stream_gg)

## Ribbon chart ---- 
ribbonchart <- function(dataInput1){
stream_group_weekday <- dataInput1 %>% 
  melt(1:11) %>% 
  group_by(weekday, variable) %>% 
  summarise(value = sum(value)) %>% 
  mutate(weekday = ordered(weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday", "Sunday")))

library(ggalluvial)
ggplot(data = stream_group_weekday,
       aes(x = weekday
           , y = value
           , alluvium = variable)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 9
                                                      ,name = 'Blues')) +
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







