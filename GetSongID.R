#Load libraries
pacman::p_load("spotifyr","jsonlite")

#Loading your stream history
df <- fromJSON("Jonas/MyData/StreamingHistory0.json") #Insert the direction here

#Getting unique artists and songs
artists <- unique(df$artistName) #List of all artists that you have listened to
songs <- unique(df$trackName) #List the songs that you have listened to

#We can use the songs vector to only look up each song once, the id can then be joined back onto your streaming history.

#Post your tokens here. get them at the spotify application
Sys.setenv(SPOTIFY_CLIENT_ID = 'c57425777fba493b9d109c48d062643b') #get this from the dashboard
Sys.setenv(SPOTIFY_CLIENT_SECRET = '349158c994084850bfe8711d80d87ce1') #post your token here
access_token <- get_spotify_access_token()

#Getting the ID for each song in the streaming history
for (i in 1:length(songs)) {
  
  #Print loop number to see progress
  print(paste0("Loop no: ",i,", of: ",length(songs)))
  
  #Get the song that we are going to lookup
  song <- songs[i]
  
  #If we cannot lookup the id based on the spotify search, then go to the next iteration
  if(nrow(spotifyr::search_spotify(q = song,type = "track",limit = 1)) == 0) next
  
  #Getting ID via searching in the spotify data
  res_temp <- spotifyr::search_spotify(q = song #Name of the song
                                       ,type = "track" #We hint that it is a track, to delimit the search
                                       ,limit = 1 #We only want the top result
  )
  
  #Append current result with previous result
  res <- if(exists("res")) { #Check if the results object exists. if the expressions returns TRUE, then append the res object
    res <- rbind(res,res_temp[,c("name","id")])
  } else { #If there is no res file, then create it and append it with the current result 
    res <- spotifyr::search_spotify(q = song,type = "track",limit = 1)
    res <- res_temp[,c("name","id")]
  }
}

#Save the file as an RDS object, you can load it into the environment using readRDS()
saveRDS(object = res
        ,file = "directory/filename.rds"#insert direction here, remember to add '.rds', example: "Jonas/SongAndID.rds"
)