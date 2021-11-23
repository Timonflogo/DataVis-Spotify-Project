## Barchart ----

### Barchart data ----

df_barchart <- function(dataInput1,current_artist = current_artist(),selected_weekday = selected_weekday()
                        ,NumArtists = 10,NumTracks = 10){
  
  #Checks if there is selected an artist. If not then:
  if (length(current_artist) != 1) {
    return(
      df_barchart <- dataInput1 %>% 
        filter(weekday %in% selected_weekday) %>% #Filter masterdata with selected weekday 
        group_by(artist_id,artistName) %>% 
        summarise(msPlayed = sum(msPlayed)) %>%
        mutate(hsPlayed = msPlayed/3600000) %>%
        ungroup() %>% 
        select(c(artistName,hsPlayed)) %>% 
        arrange(desc(hsPlayed)) %>% 
        top_n(n = ifelse(test = is.blank(NumArtists),yes = 10,no = NumArtists))
    )
  }
  
  #Else the following is run:
  df_barchart <- dataInput1 %>% 
    filter(weekday %in% selected_weekday) %>% #Filter masterdata with selected weekdays 
    filter(artistName %in% current_artist) %>% 
    group_by(track_id,trackName) %>% 
    summarise(msPlayed = sum(msPlayed)) %>% 
    mutate(hsPlayed = msPlayed/3600000) %>% 
    ungroup() %>%
    select(c(trackName,hsPlayed)) %>% 
    arrange(desc(hsPlayed)) %>% 
    top_n(n = ifelse(test = is.blank(NumTracks),yes = 10,no = NumTracks))
  
  #Return the data frame which is used for the plot
  return(df_barchart)
}

### Barchart plot ----

barchart <- function(dataInput1,current_artist = current_artist()){
  
  #Creating a dataframe where the variables are fixed to be named x and y
  d <- as.data.frame(dataInput1) %>% setNames(nm = c("x", "y"))
  
  #Plotting the barchart
  cchart <- plot_ly(data = d
          ,x = ~reorder(x,desc(y)), y = ~y
          ,type = "bar"
          ,marker = list(color = 'rgb(65, 10, 204)')
  )
  cchart <- cchart %>% 
    layout(title = if(length(current_artist) != 1){'Top artists'} else {paste("Top tracks for",current_artist)}) %>%  #go for current Artist unless else is selected
    layout(plot_bgcolor='#E6E6E6') %>% 
    layout(paper_bgcolor='#E6E6E6') %>% 
    layout(xaxis = list(title = if(length(current_artist) != 1){'Artist'} else {'Track'}))
  
  
}