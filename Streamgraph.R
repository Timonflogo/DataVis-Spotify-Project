# libraries
require("remotes")
remotes::install_github("hrbrmstr/streamgraph")
pacman::p_load(reshape2,ggplot2,ggstream,plotly,streamgraph,RColorBrewer,Hmisc,dplyr,gridExtra,stringr)


## Streamgraph data ----
df <- readRDS("R dataframe/stream_selected_c_clean.rds")

stream_df <- df %>% 
  melt(1:10) %>%  #Keep columns 1 - 9, create a row entry for each column value of 10 - 18
  group_by(start_day_week,variable) %>% 
  summarise(value = sum(value))

stream_gg <- df %>% 
  melt(1:10) %>%  #Keep columns 1 - 9, create a row entry for each column value of 10 - 18
  group_by(year_week,variable) %>% 
  summarise(value = sum(value))

stream_gg <- as.data.frame(stream_gg)

## Plotting ----
cap_space <- function(string){capitalize(str_replace(string,pattern = "_",replacement = " "))}

ggplot(stream_gg, aes(x = year_week, y = value, fill = cap_space(string = variable))) +
  geom_stream() +
  geom_stream_label(aes(label = cap_space(string = variable))) + 
  theme(panel.background = element_blank()) +
  labs(fill = 'Features')




# streamgraph::streamgraph(stream_df, key="variable", value="value", date="start_day_week", height="600px", width="1800px") %>% 
#   #sg_fill_brewer("Spectral") %>% #BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
#   sg_legend(show = TRUE,"Features")




