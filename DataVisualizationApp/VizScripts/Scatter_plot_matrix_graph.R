## Libraries
pacman::p_load("jsonlite"
               , "dplyr"
               , "plotly"
)

time_listened <- readRDS("Data/stream_selected_c_clean.rds") %>% 
  group_by(artistName
           , trackName) %>% 
  summarise(time_played_hours = sum(msPlayed) / 3.6e+6)

d <- readRDS("Data/stream_selected_c_clean.rds") %>% 
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

d <- d %>% 
  left_join(time_listened
            , by = c('artistName' = 'artistName'
                     , 'trackName' = 'trackName')) %>% 
  mutate(bin = ntile(time_played_hours, 5))

pl_colorscale = list(c(0.0, '#119dff'),
                     c(0.5, '#119dff'),
                     c(0.5, '#ef553b'),
                     c(1, '#ef553b'))

axis = list(showline=FALSE,
            zeroline=FALSE,
            gridcolor='#ffff',
            ticklen=4,
            titlefont=list(size=13))

test <- d %>%
  plot_ly(split = ~ as.factor(bin)) 

test <- test %>%
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
      #color = ~ bin,
      colorscale = pl_colorscale,
      size = 5,
      opacity=0.5,
      line = list(
        width = 1,
        color = 'rgb(230,230,230)'
      )
    )
  ) 
test





