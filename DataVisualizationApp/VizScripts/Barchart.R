

# 
# x = weeks

df_barchart <- df %>% 
  group_by(artist_id,track_id) %>% 
  summarise(sum = sum(msPlayed))
