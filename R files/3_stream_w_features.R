## Libraries
pacman::p_load("jsonlite"
               , "dplyr"
               , "tidyr"
               , "spotifyr"
               , "pbapply"
               , "stringr"
               , "data.table"
               , "lubridate"
)


source("MyKey.R")

# Sys.setenv(SPOTIFY_CLIENT_ID = 'a0299cfb25944ecdbffc9079b987ff9a')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f773ae310efa4f74a751bb6f50d4b0b4')
# access_token <- get_spotify_access_token()

## Loading streaming history ----
address <- "Tomas data/StreamingHistory"
file_type <- ".json"

paths <- list()
for (i in seq(1,6)){
  paths[[i]] <- paste(address, i-1, file_type, sep = "")
}

list_df <- list()
df <- data.frame()
for (i in seq(from = 1, to = length(paths))) {
  print(paths[[i]])
  list_df[[i]] <- jsonlite::fromJSON(txt = paths[[i]], flatten = TRUE)
  #assign(paste0("Stream_history_", i), as.data.frame(list_df[[i]])) # if each list should be saved as a separate dataframe
}
df <- do.call(rbind, list_df)
df <- df %>% 
  mutate(track_artist = paste(trackName, artistName, sep = " "))

## Loading features of unique songs that the user listened to
track_features <- readRDS('R_dataframe/get_track_features.RDS')

## Joining features on stream history
stream_full_features <- df %>% 
  inner_join(track_features, by = "track_artist")

# filter out songs having ms played longer than ms duration
stream_full_features <- stream_full_features[stream_full_features$msPlayed != 0,]
stream_full_features <- stream_full_features[stream_full_features$msPlayed <= stream_full_features$duration_ms,]

# splitting artist_id into first id and the rest 
temp_artist <- str_split(string = stream_full_features$artist_id
                         , pattern = ","
                         , n = 2
                         , simplify = TRUE) %>% #splitting artist_id into 2 columns - main artist and the rest
  as.data.frame() %>% 
  select(1) %>% # selecting the first artist
  rename('artist_id' = "V1") %>% # Renaming the column back to artist_id
  unlist() %>% # unlist in order to use str_replace 
  # Cleaning the artist_id from 'c' and brackets
  str_replace(pattern = 'c\\("' 
              , replacement = "") %>% 
  str_replace(pattern = '"'
              , replacement = "") %>% 
  as.data.frame() %>% # Setting it back into a dataframe
  rename('artist_id' = ".") #renaming it back to artist_id

# Replacing artist_id in stream_full_features with temp_artist
stream_full_features <- stream_full_features %>% 
  mutate(artist_id = temp_artist$artist_id)

# Selecting columns to work with since some are irrelevant
stream_selected_c <- stream_full_features %>% 
  select(artist_id
         ,artistName
         ,track_id
         ,trackName
         ,msPlayed
         ,endTime
         ,duration_ms
         ,danceability
         ,energy
         ,loudness
         ,speechiness:tempo) %>%
  # standardizing features into range of 0 - 1
  mutate_at(scales::rescale
            , .vars = vars(danceability:tempo))

# Calculating the features per ms to have a single unit for aggregation
stream_selected_c <-
  setDT(stream_selected_c)[, paste0(names(stream_selected_c)[which(names(stream_selected_c)== "danceability"):which(names(stream_selected_c)== "tempo")]
                                    , "_per_ms") := lapply(.SD, `/`, stream_selected_c$duration_ms)
                           , .SDcols = danceability:tempo]
# Calculating the features value based on how long the track was actually played 
stream_selected_c <-
  setDT(stream_selected_c)[, paste0(names(stream_selected_c)[which(names(stream_selected_c)== "danceability"):which(names(stream_selected_c)== "tempo")]
                                    , "_exposed") := lapply(.SD, `*`, stream_selected_c$msPlayed)
                           , .SDcols = danceability_per_ms:tempo_per_ms]
# replacing inf vcalues with 0 
stream_selected_c <- stream_selected_c %>% 
  mutate_all(function(x) ifelse(is.infinite(x), 0, x))

# Creating date and time columns from endTime
stream_selected_c <- stream_selected_c %>% 
  mutate(date = as.Date(endTime)
         , time = format(as.POSIXct(endTime
                                    , format = "%Y-%m-%d %H:%M")
                         , "%H:%M:%S")) %>% 
  mutate(weekday = weekdays(date)) %>% # extracting weekdays form the date
  mutate(year_week = as.numeric(strftime(date, format = "%Y%W")) + 1) %>% 
  mutate(start_day_week = floor_date(date,unit = "week", week_start = 1)) %>% 
  mutate(month = months(date)) %>% 
  relocate(c(date
             , month
             , year_week
             , weekday
             , start_day_week
             , time) #change the position of these 2 columns 
             , .before = artist_id) %>% 
  relocate(c(track_id
             , artist_id)
           , .before = trackName) %>% 
  #select(-c(danceability:tempo_per_ms)) %>% 
  inner_join(readRDS('DataVisualizationApp/Data/artist_genre_split.rds')
             , by = 'artist_id') %>% 
  relocate(genre
           , .after = artistName)
  
# artist_genre <- readRDS('DataVisualizationApp/Data/artist_genre_split.rds') %>%
#   inner_join(stream_selected_c
#              , by = 'artist_id')
# 
# 
# 
# saveRDS(stream_selected_c, file = "R_dataframe/stream_selected_c_clean.rds")


