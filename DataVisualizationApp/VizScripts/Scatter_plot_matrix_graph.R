scater_plot_f <- function(dataInput1
         , probs_range_start # between 0.1 and 1
         , probs_range_end # between 0.1 and 1
         , time_played_start # between 0 and max(d$time_played_minutes)
         , time_played_end # between 0 and max(d$time_played_minutes)
         , opacity_red # between 0 and 1
         , opacity_blue # between 0 and 1
         , selected_weekday = selected_weekday() # the weekday filter
         )
{
  ## Libraries
  pacman::p_load("dplyr"
                 , "plotly"
  )
  options(scipen=999)
  # dataframe for time listened per each song in minutes
  time_listened <- dataInput1 %>% 
    filter(weekday %in% selected_weekday) %>% #Filter masterdata with selected weekday %>% 
    group_by(artistName
             , trackName) %>% 
    summarise(time_played_minutes = sum(msPlayed) / 60000) %>% 
    ungroup() %>% 
    mutate(time_played_minutes = round(time_played_minutes,2)) 
  
  # dataframe with unique tracks and artists with the features
  d <- dataInput1 %>% 
    filter(weekday %in% selected_weekday) %>% #Filter masterdata with selected weekday %>% 
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
  
  # Joining time listened dataframe with unique tracks and artists
  df_scatter <- d %>% # final dataframe for the plot
    left_join(time_listened,
              by = c('artistName' = 'artistName'
                     , 'trackName' = 'trackName')) %>%
    # filter for selecting quantiles
    mutate(
      bin = ifelse(time_played_minutes > quantile(time_played_minutes, probs =  probs_range_start) &
                     time_played_minutes <= quantile(time_played_minutes, probs = probs_range_end),
                   # necessary for distinct 2 colors in scatterplot
                   yes = 'rgba(255, 0, 0, 0.5)', # red
                   no = 'rgba(135, 206, 250, 0.5)' # blue
                   )) %>% 
    mutate(bin = factor(bin, levels = c('rgba(255, 0, 0, 0.5)' # creating the colors as factor
                                        , 'rgba(135, 206, 250, 0.5)'))
           ) %>%
    # filter for opacity 
    mutate(opacity =  ifelse(bin == 'rgba(255, 0, 0, 0.5)'
                             , yes =  opacity_red #opacity_red
                             , no = opacity_blue)) %>%
    mutate(description = ifelse(test = bin == 'rgba(255, 0, 0, 0.5)'
                                # text that is shown in the legend
                                , yes = 'Tracks in the selected range' # text shown for red color
                                , no = 'Other tracks') # text shown for blue color
           ) %>% 
    mutate(description = factor(description
                                , levels = c('Tracks in the selected range'
                                             , 'Other tracks'))) %>% 
    # filter for time played
    filter(time_played_minutes > time_played_start & time_played_minutes <= time_played_end)
  
  # specifying features of each individual plot in the scatter plot
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
  
  # setting the background of the overall scatterplot 
  fig <- fig %>%
    layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)",
           fig_bgcolor   = "rgba(0, 0, 0, 0)")
  
# loop for creating 1 plot for selected tracks, 1 for others
  # then these 2 plots are merged into 1 by add_trace
  for (i in unique(df_scatter$bin)) {
    fig <- add_trace(
      fig,
      data = df_scatter[df_scatter$bin == i,], # dataframe for each plot
      type = 'splom', # specifying scatter plot matrix type of plot
      dimensions = list( # selected variables for the plot and the values
        list(label = 'danceability', values = ~ danceability),
        list(label = 'energy', values = ~ energy),
        list(label = 'loudness', values = ~ loudness),
        list(label = 'speechiness', values = ~ speechiness),
        list(label = 'acousticness', values = ~ acousticness),
        list(label = 'instrumentalness', values = ~ instrumentalness),
        list(label = 'liveness', values = ~ liveness),
        list(label = 'valence', values = ~ valence),
        list(label = 'tempo' , values = ~ tempo)
      )
      # necessary for having a legend showing 2 categories - selected and other tracks
      , name = levels(df_scatter$description)[levels(df_scatter$bin) == i]
      # names of the tracks when hovering over, otherwise it shows the score
      , text =  ~ factor(trackName, labels = unique(trackName))
      , diagonal = list(visible = F), # deactivate diagonals
      showupperhalf = F # deactivate upper half of scatter
      , marker = list(
        color = ~ bin # defining colors for each category of the bins - selected and other tracks
        # , group = ~ bin
        # opacity for the scatterplot
        , opacity = ~ opacity
        , colorscale = pl_colorscale
        # size of the dots
        , size = 5
        # width of the lines of the dots and color of the lines 
        , line = list(width = 1,
                    color = 'rgb(230,230,230)')
      )
    )
  }
  
  scatter_plot <- fig %>%
    layout(
      title = "Scatterplot Matrix of track features",
      # hover functionality
      hovermode = 'closest',
      dragmode = 'select',
      # show the legend
      showlegend = T, 
      # background color of each individual plot in the scatter plot
      plot_bgcolor = 'rgba(240,240,240, 0.95)',
      xaxis = list(
        domain = NULL,
        showline = F,
        zeroline = F,
        gridcolor = '#ffff',
        ticklen = 4,
        titlefont = list(size = 13)
      )
      , yaxis1 = list(
        domain = NULL,
        showline = F,
        zeroline = F,
        gridcolor = '#ffff',
        ticklen = 4,
        titlefont = list(size = 13)
      )
      , xaxis2 = axis,
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






