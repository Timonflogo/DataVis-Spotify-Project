library(lubridate)
library(dplyr)
library(ggplot2)

df_weekday_heatmap <- function(dataInput1){

# dataInput1 <- readRDS("Data/stream_selected_c_clean.rds")

df_heatmap <- dataInput1 %>% 
  group_by(weekday
           , time) %>% 
  summarise(mPlayed = sum(msPlayed) / 60000) %>% 
  ungroup() %>% 
  mutate(time = substr(time
                       , start = 1
                       , stop = 2)) %>% 
  arrange(weekday, time, mPlayed) %>% 
  group_by(weekday
           , time) %>% 
  summarise(mPlayed = sum(mPlayed)) %>% 
  ungroup() 
}

sevenDHeatmap <- function(data = df_weekday_heatmap()){
total_time <- expand.grid(c(0:6),c(0:23))
total_time$Var2 <- ifelse(nchar(total_time$Var2) == 1,paste0('0',total_time$Var2),no = total_time$Var2)
total_time <- total_time %>% 
  mutate(time = paste(Var2, ":00", sep = "")) %>% 
  arrange(Var1, Var2) %>% 
  mutate(index = seq(from = 0
                     , to = (24 * 7) - 1
                     , by = 1)) %>% 
  rename(weekday = Var1
         , hour = Var2) %>% 
  mutate(day_of_week = recode(weekday, "0" = "Monday"
                              , "1" = "Tuesday"
                              , "2" = "Wednesday"
                              , "3" = "Thursday"
                              , "4" = "Friday"
                              , "5" = "Saturday"
                              , "6" = "Sunday")) %>% 
  select(index, day_of_week, time, hour)

data <- total_time %>% #We use total time, to preserve empty minutes as empty
  left_join(data,by = c("day_of_week" = "weekday",
                        "hour" = "time"))

xaxis <- total_time %>%
  mutate(rn2 = 1:n()) %>% 
  group_by(day_of_week) %>% 
  mutate(rn = 1:n()) %>% 
  filter(rn == 1)

ggplot(data, aes(x = index,y = 1, fill = mPlayed)) + 
  geom_tile() +
  #scale_fill_manual(values = RColorBrewer::brewer.pal(n = 12,name = 'Set3')) +
  theme(plot.background = element_rect(fill = "#ffffff", colour = "#ffffff")
        ,panel.background = element_rect(fill = "#ffffff", colour = "#ffffff") 
        ,panel.grid.major.x = element_line(size = 0.3, color="black",linetype = "dotted")
        ,panel.grid.minor = element_blank() #Remove gridlines
        ,panel.grid.major.y = element_blank() #Remove gridlines
        ,axis.text.x = element_text(angle = 0)
        ,axis.ticks = element_blank()
        ,axis.ticks.y = element_blank()
        ,axis.text.y = element_blank()
        ,legend.background = element_rect(fill = "#ffffff")
        ,plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(labels = xaxis$day_of_week #Vector of labels
                     ,breaks = xaxis$rn2
  ) +
  scale_fill_gradient2(low = "snow1", high = "navyblue") +
  labs(fill = 'Time played',x = "Day of the week",y = "Minutes played")  +
  ggtitle("Density of streaming time per week")
}