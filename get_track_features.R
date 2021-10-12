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

#loading the data
artist_id <- readRDS(file = 'artist_id.RDS')
df_unique_artist_name <- readRDS(file = 'df_unique_artist_name.RDS')
df_unique_track_id <- readRDS(file = 'df_unique_track_id.RDS')

# joining all 3 dataframes into 1 
joined_df <- artist_id %>% 
  left_join(df_unique_artist_name
            , by = 'track_artist') %>% 
  left_join(df_unique_track_id
            , by = 'track_artist') 

# function for extracting all features of each song in the list based on the id
f <- function(track_feature){
  
  if(length(spotifyr::get_track_audio_features(id = track_feature)) != 0){
    spotifyr::get_track_audio_features(id = track_feature)
  } else {
    NULL
  }
}

# empty dataframe to save the track features
df_track_features <- data.frame()

# Running the function and saving it into the dataframe
system.time({df_track_features <- pbsapply(X = joined_df$track_id,FUN = f)}, gcFirst = TRUE)

df <- do.call(rbind,df_track_features)


df_track_feature <- data.frame()
for (i in seq(from = 1, to = length(df_track_features))) {
  
  #Print current entry
  print(i)
  
  #Control if the track has 18 features
  if(length(df_track_features[[i]]) != 18) {
    print(paste0("skip track: ",i))
    next
  }
  df_track_feature <- rbind(df_track_feature,df_track_features[[i]])
}

df_track_features <- df_track_feature

# Rename the column id to track_id for easier join
df_track_features <- df_track_features %>% 
  as.data.frame() %>% # specify as a dataframe because t() creates a matrix
  rename('track_id' = 'id') #rename the column for later join

# Joining the track features table with the main joined table
main_df <- joined_df %>% 
  left_join(df_track_features
            , by = 'track_id')

# saveRDS(df_track_features, file = "get_track_features.rds")







