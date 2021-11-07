scater_plot_f <- function(dataInput1
         , probs_range_start # between 0.1 and 1
         , probs_range_end # between 0.1 and 1
         # , time_played_start # between 0 and max(d$time_played_minutes)
         # , time_played_end # between 0 and max(d$time_played_minutes)
         , opacity_red # between 0 and 1
         , opacity_blue # between 0 and 1
         )
{
  ## Libraries
  pacman::p_load("dplyr"
                 , "plotly"
  )
  
  # dataInput1 <- readRDS("Data/stream_selected_c_clean.rds") 
  
  options(scipen=999)
  time_listened <- dataInput1 %>% 
    group_by(artistName
             , trackName) %>% 
    summarise(time_played_minutes = sum(msPlayed) / 60000) %>% 
    ungroup() %>% 
    mutate(time_played_minutes = round(time_played_minutes,2)) 
  
  
  
  d <- dataInput1 %>% 
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
    #@replace the probs with userInput #########################################################
  mutate(bin = ifelse(time_played_minutes > quantile(time_played_minutes,probs =  probs_range_start) & time_played_minutes <= quantile(time_played_minutes,probs = probs_range_end) 
                      ,yes = '#FF0000' # red
                      ,no = '#0000FF')) %>% # blue
    mutate(bin = factor(bin, levels = c('#FF0000', '#0000FF'))) %>% 
    mutate(opacity =  ifelse(bin == '#FF0000',yes =  opacity_red, no = opacity_blue)) %>% 
    #@ replace the number of minutes with user inoput
    filter(time_played_minutes > 5 & time_played_minutes <= 100)
  
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
    layout(plot_bgcolor='#E6E6E6') %>% 
    layout(paper_bgcolor='#E6E6E6') %>% 
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
  
}
# scater_plot_f(dataInput1 = readRDS("Data/stream_selected_c_clean.rds") 
#               , probs_range_start = 0.2
#               , probs_range_end = 0.6
#               , time_played_start = 5
#               , time_played_end = 100
#               , opacity_red = 1
#               , opacity_blue = 0.1)






