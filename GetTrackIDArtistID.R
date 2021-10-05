## Libraries
pacman::p_load("jsonlite"
               , "dplyr"
               , "tidyr"
               , "spotifyr"
               , "pbapply"
               )


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


## Get song ID ----

f <- function(track_artist){
  
  if(length(spotifyr::search_spotify(q = track_artist,type = "track",limit = 1)) != 0){
    spotifyr::search_spotify(q = track_artist,type = "track",limit = 1)[[1]][[1]]$name
  } else {
    NULL
  }
  
  #spotifyr::search_spotify(q = track_artist,type = "track",limit = 1)[[1]][[1]]$name
  # result_artist_id <- spotifyr::search_spotify(q = track_artist,type = "track",limit = 1)[[1]][[1]]$id
  # result_track_id <- spotifyr::search_spotify(q = track_artist,type = "track",limit = 1)$id
  #cbind(result_artist_name,result_artist_id,result_track_id)
}

system.time({df_unique$artist_name <- pblapply(X = df_unique$track_artist,FUN = f)}, gcFirst = TRUE)





