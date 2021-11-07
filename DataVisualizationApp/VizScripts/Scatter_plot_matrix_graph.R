## Libraries
pacman::p_load("dplyr"
               , "plotly"
)

df <- readRDS("Data/stream_selected_c_clean.rds") 

options(scipen=999)
time_listened <- df %>% 
  group_by(artistName
           , trackName) %>% 
  summarise(time_played_minutes = sum(msPlayed) / 60000) %>% 
  ungroup() %>% 
  mutate(time_played_minutes = round(time_played_minutes,2)) 



d <- df %>% 
  distinct(artistName
           , trackName
           , duration_ms
           , danceability
           , energy
           , loudness
           , speechiness
           , acousticness
           , instrumentalness
           , liveness
           , valence
           , tempo)

# percent_select <- round(nrow(time_listened) * 0.1 # replace to be reactive
#                         ,0)

d <- d %>% 
  left_join(time_listened
            , by = c('artistName' = 'artistName'
                     , 'trackName' = 'trackName')) %>% 
  mutate(bin = ifelse(time_played_minutes > quantile(time_played_minutes,probs = 0.9) & time_played_minutes <= quantile(time_played_minutes,probs = 1)
                      ,yes = '#FF0000',no = '#0000FF')) %>% 
  mutate(bin = factor(bin, levels = c('#FF0000', '#0000FF'))) %>% 
  mutate(opacity =  ifelse(bin == '#FF0000', 1, 0.2)) %>% 
  filter(time_played_minutes > 5)

pl_colorscale = list(c(0.0, '#119dff'),
                     c(0.5, '#119dff'),
                     c(0.5, '#ef553b'),
                     c(1, '#ef553b'))

axis = list(showline=FALSE,
            zeroline=FALSE,
            gridcolor='#ffff',
            ticklen=4,
            titlefont=list(size=13))

scatter_plot <- d %>%
  plot_ly()

scatter_plot <- scatter_plot %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label='danceability', values = ~ danceability)
      , list(label='energy', values = ~ energy)
      , list(label='loudness', values = ~ loudness)
      , list(label='speechiness', values = ~ speechiness)
      , list(label='acousticness', values = ~ acousticness)
      , list(label='instrumentalness', values = ~ instrumentalness)
      , list(label='liveness', values = ~ liveness)
      , list(label='valence', values = ~ valence)
      , list(label = 'tempo' , values = ~ tempo)
    ),
    text = ~ factor(trackName, labels = unique(trackName)),
    diagonal=list(visible=F),
    marker = list(
      color = ~ bin,
      group = ~ bin,
      colorscale = pl_colorscale,
      size = 5,
      opacity=d$opacity,
      line = list(
        width = 1
        #, color = 'rgb(0,230,230)'
      )
    )
  ) 
scatter_plot





