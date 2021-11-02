d <- readRDS('Data/stream_selected_c_clean.rds')
library(dplyr)
library(lubridate)
library(hms)

# total_time_played <- function(dataInput1){
#   round(seconds_to_period(sum(dataInput1$msPlayed) / 1000), 0)
# }
# total_time_played <- round(seconds_to_period(sum(d$msPlayed) / 1000), 0)

total_songs_played <- function(d){
  total_time_played <- round(seconds_to_period(sum(d$msPlayed) / 1000), 0)
  print(paste('You have played'
              , nrow(d)
              , 'songs which is equal to'
              , total_time_played))
}
total_songs_played(d)

# total_songs_played <- nrow(d)

total_unique_songs_played <- function(dataInput1){
  unique_songs <- dataInput1 %>% 
    group_by(artistName
             , trackName) %>% 
    summarise(freq = n()) %>% 
    ungroup() %>% 
    nrow()
  print(paste('You have listened to'
              , unique_songs
              , 'different songs'))
}
# total_unique_songs_played <- d %>% 
#   group_by(artistName
#            , trackName) %>% 
#   summarise(freq = n()) %>% 
#   ungroup() %>% 
#   nrow()

total_unique_artist_played <- function(dataInput1){
  uniq_artist <- dataInput1 %>% 
    group_by(artistName
             , trackName) %>% 
    summarise(freq = n()) %>% 
    select(artistName) %>% 
    unique() %>% 
    nrow()
  print(paste('You have listened to'
              , uniq_artist
              , 'different artists'))
}

# total_unique_artist_played <- d %>% 
#   group_by(artistName
#            , trackName) %>% 
#   summarise(freq = n()) %>% 
#   select(artistName) %>% 
#   unique() %>% 
#   nrow()

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
  print(paste('Your favourite artist'
              , artist_name
              , 'was played '
              , freq_played
              , 'times and in total for'
              , total_time_played))
}

# favourite_artist <- d %>%
#   group_by(artistName) %>%
#   summarise(freq_played = n()
#             , total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>%
#   mutate(total_time_played = round(total_time_played,0)) %>% 
#   arrange(desc(freq_played)) %>%
#   top_n(1)

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
  print(paste('Your most listned track is'
              , trackName
              , 'from'
              , artistName
              , 'and it was listened for'
              , freq_played
              , 'times equivalently to '
              , total_time_played))
}

# favourite_track <- d %>%
#   group_by(trackName, artistName) %>%
#   summarise(freq_played = n()
#             , total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>%
#   ungroup() %>% 
#   mutate(total_time_played = round(total_time_played,0)) %>% 
#   arrange(desc(total_time_played)) %>% 
#   top_n(1)




# Year - the release year of the recording. Note that due to vagaries of releases, re-releases, re-issues and general madness, sometimes the release years are not what you'd expect.
# Energy - The energy of a song - the higher the value, the more energtic song
# Danceability - The higher the value, the easier it is to dance to this song.
# Loudness (dB) - The higher the value, the louder the song.
# Liveness - The higher the value, the more likely the song is a live recording.
# Valence - The higher the value, the more positive mood for the song.
# Length - The duration of the song.
# Acousticness - The higher the value the more acoustic the song is.
# Speechiness - The higher the value the more spoken word the song contains.
# Duration - The length of the song.
