## Libraries
pacman::p_load("jsonlite"
               , "dplyr"
               , "tidyr"
               , "spotifyr"
               , "pbapply"
)

Sys.setenv(SPOTIFY_CLIENT_ID = 'a0299cfb25944ecdbffc9079b987ff9a')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f773ae310efa4f74a751bb6f50d4b0b4')
access_token <- get_spotify_access_token()

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
track_features <- readRDS('get_track_features.RDS')

## Joining features on stream history
stream_full_features <- df %>% 
  inner_join(track_features, by = "track_artist")


