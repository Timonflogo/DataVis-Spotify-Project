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

## Concatenating track and artist
df <- df %>% 
  mutate(track_artist = paste(df$trackName, df$artistName, sep = " "))


## Unique track and song combinations
df_unique <- data.frame(track_artist = unique(df$track_artist))

## Get IDs ----

f_artist_id <- function(track_artist){
  
  if(length(spotifyr::search_spotify(q = track_artist,type = "track",limit = 1)) != 0){
    spotifyr::search_spotify(q = track_artist,type = "track",limit = 1)[[1]][[1]]$id
  } else {
    NULL
  }
}

f_artist_name <- function(track_artist){
  
  if(length(spotifyr::search_spotify(q = track_artist,type = "track",limit = 1)) != 0){
    spotifyr::search_spotify(q = track_artist,type = "track",limit = 1)[[1]][[1]]$name
  } else {
    NULL
  }
}

f_track_id <- function(track_artist){
  
  if(length(spotifyr::search_spotify(q = track_artist,type = "track",limit = 1)) != 0){
    spotifyr::search_spotify(q = track_artist,type = "track",limit = 1)$id
  } else {
    NULL
  }
}

#running the functions
system.time({df_unique$artist_id <- pblapply(X = df_unique$track_artist,FUN = f_artist_id)}, gcFirst = TRUE)
system.time({df_unique$artist_name <- pblapply(X = df_unique$track_artist,FUN = f_artist_name)}, gcFirst = TRUE)
system.time({df_unique$trackid <- pblapply(X = df_unique$track_artist,FUN = f_track_id)}, gcFirst = TRUE)

# saveRDS(df_unique, file = "artist_id.RDS")


