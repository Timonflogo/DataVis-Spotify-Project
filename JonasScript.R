

library(jsonlite)

df <- fromJSON("Jonas/MyData/StreamingHistory0.json")

artists <- unique(df$artistName)
songs <- unique(df$trackName)


library(spotifyr)
Sys.setenv(SPOTIFY_CLIENT_ID = 'c57425777fba493b9d109c48d062643b') #get this from the dashboard
Sys.setenv(SPOTIFY_CLIENT_SECRET = '349158c994084850bfe8711d80d87ce1') #get this from the dashboard
access_token <- get_spotify_access_token()


for (i in 1:nrow(df)) {
  
  print(i)
  
  song <- df[i,"trackName"]
  
  if(nrow(spotifyr::search_spotify(q = song,type = "track",limit = 1,include_meta_info = F)) == 0) next
  
  res_temp <- spotifyr::search_spotify(q = song,type = "track",limit = 1,include_meta_info = F)
  
  res <- if(exists("res")) {
    res <- rbind(res,res_temp[,c("name","id")])
  } else {
    res <- spotifyr::search_spotify(q = song,type = "track",limit = 1,include_meta_info = F)
    res <- res_temp[,c("name","id")]
  }
}

saveRDS(object = res,file = "Jonas/SongAndID")


t <- spotifyr::get_track(res$id[1])



