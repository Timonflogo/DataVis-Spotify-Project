stream_group_weekday <- df %>% 
  melt(1:11) %>% 
  filter(date >= "2020-09-23" & date <= "2020-10-07") %>% 
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









