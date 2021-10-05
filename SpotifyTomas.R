# Libraries 
pacman::p_load("jsonlite"
               , "dplyr"
               , "tidyr"
               , "spotifyr")

# Load streaming history ----
address <- "/Users/tomasbui/Library/Mobile Documents/com~apple~CloudDocs/Downloads/MyData/StreamingHistory"
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
head(df)

# Spotifyr set up ---- 
Sys.setenv(SPOTIFY_CLIENT_ID = 'a0299cfb25944ecdbffc9079b987ff9a')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f773ae310efa4f74a751bb6f50d4b0b4')
access_token <- get_spotify_access_token()

# Creating a new column in df with artist and track name
df <- df %>% 
  mutate(artist_track = paste(df$trackName, df$artistName, sep = " "))

# Manual search using merged artist and track name through all availabel spotify songs - to get song ID.
# The search looks 
spotify_search <- search_spotify(q = 'Waves - Timbaland Remix Dean Lewis ', type = 'track', limit = 1)

# Get features of a specific track
track_features <- get_track_audio_features()