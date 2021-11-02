d <- readRDS('DataVisualizationApp/Data/stream_selected_c_clean.rds')
library(dplyr)
library(lubridate)
library(hms)


# time_played_per_month <- d %>%
#   group_by(month) %>%
#   summarise(total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>%
#   mutate(total_time_played = round(total_time_played, 0)) %>%
#   arrange(total_time_played)
# 
# favourite_artist <- d %>%
#   group_by(artistName) %>%
#   summarise(total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>%
#   mutate(total_time_played = round(total_time_played, 0)) %>%
#   top_n(1)
# 
# time_played_per_weekdays <- d %>%
#   group_by(weekday) %>%
#   summarise(total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>%
#   mutate(total_time_played = round(total_time_played, 0)) %>%
#   arrange(desc(total_time_played))
# 
# favourite_artist <- d %>%
#   group_by(artistName) %>%
#   summarise(freq_played = n()
#             , total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>%
#   mutate(total_time_played = round(total_time_played,0)) %>% 
#   arrange(desc(freq_played)) %>%
#   top_n(1)
# 
# favourite_track <- d %>%
#   group_by(trackName, artistName) %>%
#   summarise(freq_played = n()
#             , total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>%
#   ungroup() %>% 
#   mutate(total_time_played = round(total_time_played,0)) %>% 
#   arrange(desc(total_time_played)) %>% 
#   top_n(1)
# 
# freq_per_track <- d %>%
#   group_by(trackName, artistName) %>%
#   summarise(freq_played = n()
#             , total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>%
#   arrange(desc(total_time_played))
# 
# total_time_played_per_year <- d %>% 
#   mutate(year = as.numeric(format(date,'%Y'))) %>% 
#   group_by(year) %>% 
#   summarise(total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>% 
#   arrange(desc(total_time_played))
# 
# freq_tracks_played_per_year <- d %>% 
#   mutate(year = as.numeric(format(date,'%Y'))) %>% 
#   group_by(year) %>% 
#   summarise(freq_played = n()
#             , total_time_played = seconds_to_period(sum(msPlayed) / 1000))
# 
# features_per_weekday <- d %>% 
#   group_by(weekday) %>% 
#   summarise(danceability = sum(danceability_exposed)
#             , energy = sum(energy_exposed)
#             , loudness = sum(loudness_exposed)
#             , speechiness = sum(speechiness_exposed)
#             , acousticness = sum(acousticness_exposed)
#             , instrumentalness = sum(instrumentalness_exposed)
#             , liveness = sum(liveness_exposed)
#             , valence = sum(valence_exposed)
#             , tempo = sum(tempo_exposed)) %>% 
#   arrange(desc(danceability))

total_time_played <- round(seconds_to_period(sum(d$msPlayed) / 1000), 0)
  # group_by(month) %>% 
  # summarise(total_time_played = round(seconds_to_period(sum(msPlayed) / 1000), 0)) %>% 
  # filter(month == lubridate::month(Sys.Date(), label = TRUE, abbr = FALSE)) 

total_songs_played <- nrow(d)

total_unique_songs_played <- d %>% 
  group_by(artistName
           , trackName) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  nrow()

total_unique_artist_played <- d %>% 
  group_by(artistName
           , trackName) %>% 
  summarise(freq = n()) %>% 
  select(artistName) %>% 
  unique() %>% 
  nrow()

favourite_artist <- d %>%
  group_by(artistName) %>%
  summarise(freq_played = n()
            , total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>%
  mutate(total_time_played = round(total_time_played,0)) %>% 
  arrange(desc(freq_played)) %>%
  top_n(1)

favourite_track <- d %>%
  group_by(trackName, artistName) %>%
  summarise(freq_played = n()
            , total_time_played = seconds_to_period(sum(msPlayed) / 1000)) %>%
  ungroup() %>% 
  mutate(total_time_played = round(total_time_played,0)) %>% 
  arrange(desc(total_time_played)) %>% 
  top_n(1)




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
