## Libraries
pacman::p_load("jsonlite"
               , "dplyr"
               , "plotly"
)

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
  plot_ly() 

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
      # color = ~Outcome,
      colorscale = pl_colorscale,
      size = 5,
      opacity=0.1,
      line = list(
        width = 1,
        color = 'rgb(230,230,230)'
      )
    )
  ) 
test





