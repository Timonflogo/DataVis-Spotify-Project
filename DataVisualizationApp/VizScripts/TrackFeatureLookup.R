
df1 <- readRDS("Data/stream_selected_c_clean.rds")

df1 <- df1 %>% 
  select(track_id,trackName,msPlayed,duration_ms,danceability_exposed:tempo_exposed) %>% 
  as.data.frame()

options(scipen = 999)
df2 <- sapply(X = 4:12,FUN = function(i) df1[,i] / df1$msPlayed * df1$duration_ms) %>% 
  as.data.frame() %>% 
  setNames(nm = c("danceability_absolute","energy_absolute","loudness_absolute","speechiness_absolute","acousticness_absolute","instrumentalness_absolute","liveness_absolute","valence_absolute","tempo_absolute"))

df_track_features <- cbind(df1,df2) %>% 
  select(track_id,trackName,danceability_absolute:tempo_absolute)



