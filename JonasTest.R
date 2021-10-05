
## load Spotify ----
pacman::p_load("spotifyr","jsonlite")

#Loading your stream history
df <- fromJSON("Jonas/MyData/StreamingHistory0.json") #Insert the direction here
df$key <- paste(df$artistName,df$trackName,sep = ":")

#Getting unique artists and songs
artists <- unique(df$artistName) #List of all artists that you have listened to
songs <- unique(df$trackName) #List the songs that you have listened to

#We can use the songs vector to only look up each song once, the id can then be joined back onto your streaming history.

#Post your tokens here. get them at the spotify application
Sys.setenv(SPOTIFY_CLIENT_ID = 'c57425777fba493b9d109c48d062643b') #get this from the dashboard
Sys.setenv(SPOTIFY_CLIENT_SECRET = '349158c994084850bfe8711d80d87ce1') #post your token here
access_token <- get_spotify_access_token()



### Load files ----


df <- fromJSON("Jonas/MyData/StreamingHistory0.json") #Insert the direction here
df$key <- paste(df$artistName,df$trackName,sep = ":")


### Using a function to get artist name ----
ids <- readRDS("Jonas/SongAndIDV2.rds")
length(unique(ids$name))

q = "2CbtdkBeW9Znt4vXTOafAl"
f <- function(q){
  spotifyr::get_track(id = q)$artists$name
}

f(q = "2CbtdkBeW9Znt4vXTOafAl")

ids$artist <- lapply(ids$id,f)

library(pbapply)
ids$artist <- pbapply(ids$id,f)

### Using pbapply

library(tidyr)
keys <- data.frame(key = unique(df$key))
separate(keys$key,sep = ";")

separate(keys$, c("A", "B"))