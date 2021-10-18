# libraries
require("remotes")
remotes::install_github("hrbrmstr/streamgraph")
pacman::p_load(reshape2,ggplot2,ggstream,plotly,streamgraph,RColorBrewer,Hmisc,dplyr,gridExtra,stringr)


## Streamgraph data ----
df <- readRDS("R dataframe/stream_selected_c_clean.rds")

stream_df <- df %>% 
  melt(1:11) %>%  #Keep columns 1 - 9, create a row entry for each column value of 10 - 18
  group_by(start_day_week,variable) %>% 
  summarise(value = sum(value))

stream_gg <- df %>% 
  melt(1:11) %>%  #Keep columns 1 - 9, create a row entry for each column value of 10 - 18
  group_by(year_week,month,variable) %>% 
  summarise(value = sum(value))

#Create an index to map the y values onto a continous x axis
stream_gg$index <- sort(rep(seq(1,55),9))


## Plotting ----

cap_space <- function(string){capitalize(str_replace(string,pattern = "_",replacement = " "))}
idx <- seq(from = 1,to = length(stream_gg$index),by = 9 * 4.45) #months are on avg. 4.5 weeks

ggplot(stream_gg, aes(x = index, y = value, fill = cap_space(string = variable))) +
  geom_stream(n_grid = 57,bw = 0.25) + #bw = wigglyness
  #geom_stream_label(aes(label = cap_space(string = variable))) + 
  theme(panel.background = element_blank()
        ,panel.grid.major.x = element_line(size = 0.3, color="black",linetype = "dotted")
        ,axis.text.x = element_text(angle = 0)
        ,axis.ticks = element_blank()
        ) +
  scale_x_continuous(labels = stream_gg$month[idx]
                     ,breaks = stream_gg$index[idx]
                     ,n.breaks = length(stream_gg$month[idx])) +
  labs(fill = 'Features',x = "Time") -> p

plot(p)

# streamgraph::streamgraph(stream_df, key="variable", value="value", date="start_day_week", height="600px", width="1800px") %>% 
#   #sg_fill_brewer("Spectral") %>% #BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
#   sg_legend(show = TRUE,"Features")




