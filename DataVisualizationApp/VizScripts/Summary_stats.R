d <- readRDS('Data/stream_selected_c_clean.rds')
library(dplyr)
library(lubridate)
library(hms)

# total_time_played <- function(dataInput1){
#   round(seconds_to_period(sum(dataInput1$msPlayed) / 1000), 0)
# }
# total_time_played <- round(seconds_to_period(sum(d$msPlayed) / 1000), 0)

total_songs_played <- function(dataInput1){
  total_time_played <- round(seconds_to_period(sum(dataInput1$msPlayed) / 1000), 0)
  paste('You have played', nrow(dataInput1), 'songs which is equal to', total_time_played)
}


total_unique_songs_played <- function(dataInput1){
  unique_songs <- dataInput1 %>% 
    group_by(artistName
             , trackName) %>% 
    summarise(freq = n()) %>% 
    ungroup() %>% 
    nrow()
  unique_songs
}

total_unique_artist_played <- function(dataInput1){
  unique_artist <- dataInput1 %>% 
    group_by(artistName
             , trackName) %>% 
    summarise(freq = n()) %>% 
    select(artistName) %>% 
    unique() %>% 
    nrow()
  unique_artist
}

favourite_artist <- function(dataInput1){
  temp <- dataInput1 %>%
    group_by(artistName) %>%
    summarise(freq_played = n()
              , total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>%
    mutate(total_time_played = round(total_time_played,0)) %>% 
    arrange(desc(freq_played)) %>%
    top_n(1)
  artist_name <- temp$artistName
  freq_played <- temp$freq_played
  total_time_played <- temp$total_time_played
  
  artist_name
  
  # print(paste('Your favourite artist'
  #             , artist_name
  #             , 'was played '
  #             , freq_played
  #             , 'times and in total for'
  #             , total_time_played))
}

favourite_track <- function(dataInput1){
  temp <- dataInput1 %>%
    group_by(trackName, artistName) %>%
    summarise(freq_played = n()
              , total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>%
    ungroup() %>% 
    mutate(total_time_played = round(total_time_played,0)) %>% 
    arrange(desc(total_time_played)) %>% 
    top_n(1)
  trackName <- temp$trackName
  artistName <- temp$artistName
  freq_played <- temp$freq_played
  total_songs_played <- temp$total_time_played
  
  trackName
  
  # print(paste('Your most listned track is'
  #             , trackName
  #             , 'from'
  #             , artistName
  #             , 'and it was listened for'
  #             , freq_played
  #             , 'times equivalently to '
  #             , total_time_played))
}
