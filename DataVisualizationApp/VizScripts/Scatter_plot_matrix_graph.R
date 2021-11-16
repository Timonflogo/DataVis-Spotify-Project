scater_plot_f <- function(dataInput1
         , probs_range_start # between 0.1 and 1
         , probs_range_end # between 0.1 and 1
         , time_played_start # between 0 and max(d$time_played_minutes)
         , time_played_end # between 0 and max(d$time_played_minutes)
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
  
  df_scatter <- d %>%
    left_join(time_listened,
              by = c('artistName' = 'artistName'
                     , 'trackName' = 'trackName')) %>%
    #@replace the probs with userInput #########################################################
  mutate(
    bin = ifelse(
      time_played_minutes > quantile(time_played_minutes
                                     , probs =  probs_range_start) &
        time_played_minutes <= quantile(time_played_minutes
                                        , probs = probs_range_end),
      yes = 'rgba(255, 0, 0, 0.5)', # red
      no = 'rgba(135, 206, 250, 0.5)'
    )
  ) %>% # blue
    mutate(bin = factor(bin, levels = c(
      'rgba(255, 0, 0, 0.5)', 'rgba(135, 206, 250, 0.5)'
    ))) %>%
    mutate(opacity =  ifelse(bin == 'rgba(255, 0, 0, 0.5)'
                             , yes =  opacity_red #opacity_red
                             , no = opacity_blue)) %>%
    mutate(description = ifelse(test = bin == 'rgba(255, 0, 0, 0.5)'
                                , yes = 'Tracks in the selected range'
                                , no = 'Other tracks')) %>% 
    mutate(description = factor(description
                                , levels = c('Tracks in the selected range'
                                             , 'Other tracks'))) %>% 
    filter(time_played_minutes > time_played_start & time_played_minutes <= 200)
  
  axis = list(
    showline = FALSE,
    zeroline = FALSE,
    gridcolor = '#ffff',
    ticklen = 4,
    titlefont = list(size = 13)
  )
  
  pl_colorscale = list(c(0.0, '#119dff'),
                       c(0.5, '#119dff'),
                       c(0.5, '#ef553b'),
                       c(1, '#ef553b'))
  
  fig <- plot_ly()
  for (i in unique(df_scatter$bin)) {
    fig <- add_trace(
      fig,
      data = df_scatter[df_scatter$bin == i,],
      type = 'splom',
      dimensions = list(
        list(label = 'danceability', values = ~ danceability),
        list(label = 'energy', values = ~ energy),
        list(label = 'loudness', values = ~ loudness),
        list(label = 'speechiness', values = ~ speechiness),
        list(label = 'acousticness', values = ~ acousticness),
        list(label = 'instrumentalness', values = ~ instrumentalness),
        list(label = 'liveness', values = ~ liveness),
        list(label = 'valence', values = ~ valence),
        list(label = 'tempo' , values = ~ tempo)
      ),
      name = levels(df_scatter$description)[levels(df_scatter$bin) == i],
      text =  ~ factor(trackName, labels = unique(trackName)),
      diagonal = list(visible = F),
      showupperhalf = F,
      marker = list(
        color = ~ bin,
        group = ~ bin,
        opacity = ~ opacity, 
        colorscale = pl_colorscale,
        size = 5,
        line = list(width = 1,
                    color = 'rgb(230,230,230)')
      )
    )
  }
  
  scatter_plot <- fig %>%
    layout(
      title = "Scatterplot Matrix",
      hovermode = 'closest',
      dragmode = 'select',
      showlegend = T, 
      plot_bgcolor = 'rgba(240,240,240, 0.95)',
      xaxis = list(
        domain = NULL,
        showline = F,
        zeroline = F,
        gridcolor = '#ffff',
        ticklen = 4,
        titlefont = list(size = 13)
      ),
      yaxis1 = list(
        domain = NULL,
        showline = F,
        zeroline = F,
        gridcolor = '#ffff',
        ticklen = 4,
        titlefont = list(size = 13)
      ),
      xaxis2 = axis,
      xaxis3 = axis,
      xaxis4 = axis,
      xaxis5 = axis,
      xaxis6 = axis,
      xaxis7 = axis,
      xaxis8 = axis,
      yaxis2 = axis,
      yaxis3 = axis,
      yaxis4 = axis,
      yaxis5 = axis,
      yaxis6 = axis,
      yaxis7 = axis,
      yaxis8 = axis,
      yaxis9 = axis
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






