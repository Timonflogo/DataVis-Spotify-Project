
df <- readRDS("Data/stream_selected_c_clean.rds")

# stream_group_date <- df %>%
#   melt(1:13) %>% # melt(which(names(dataInput1) == "track_id"):which(names(dataInput1) == "duration_ms")) %>%
#   group_by(date, month, variable) %>%
#   summarise(value = sum(value))
# stream_group_date$index <- sort(rep(seq(1,nrow(stream_group_date)/length(unique(stream_group_date$variable))),9)) #Create an index to map the y values onto a continous x axis

streamgraph_df <- function(removeOther = removeOther()){
  x <- df %>%
    select(date,month,artist_id,artistName,duration_ms) %>% 
    mutate(year = substr(date,1,4)) %>% 
    group_by(date,year,month,artist_id,artistName) %>% 
    dplyr::summarize(value = sum(duration_ms))
  
  topart <- x %>% 
    group_by(artist_id,artistName) %>% 
    dplyr::summarise(value = sum(value)) %>% 
    ungroup() %>% 
    slice_max(n = 11,order_by = value) #N the amount of top artists
  
  if (removeOther) {
    stream_group_date_artist <- x %>% 
      left_join(y = topart,by = "artist_id") %>% 
      dplyr::rename(category = artistName.y,msPlayed = value.x,artistName = artistName.x) %>% 
      select(-value.y) %>% 
      mutate(category = replace_na(category, "Other")) %>% 
      filter(category != 'Other') %>% #Filter masterdata with selected weekday
      group_by(date,year,month,category) %>% 
      dplyr::summarise(msPlayed = sum(msPlayed)
                ,hsPlayed = sum(msPlayed)/3600000)
    
    stream_group_date_artist$index <- as.integer(factor(stream_group_date_artist$date))
    
    return(stream_group_date_artist)
  }

  stream_group_date_artist <- x %>% 
    left_join(y = topart,by = "artist_id") %>% 
    dplyr::rename(category = artistName.y,msPlayed = value.x,artistName = artistName.x) %>% 
    select(-value.y) %>% 
    mutate(category = replace_na(category, "Other")) %>% 
    #filter(category != 'Other') %>% #Filter masterdata with selected weekday
    group_by(date,year,month,category) %>% 
    dplyr::summarise(msPlayed = sum(msPlayed)
                     ,hsPlayed = sum(msPlayed)/3600000)
  
  stream_group_date_artist$index <- as.integer(factor(stream_group_date_artist$date))
  
  return(stream_group_date_artist)
}