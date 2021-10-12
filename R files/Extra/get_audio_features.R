

## Laoding spotfiyr ----
Sys.setenv(SPOTIFY_CLIENT_ID = 'c57425777fba493b9d109c48d062643b') #get this from the dashboard
Sys.setenv(SPOTIFY_CLIENT_SECRET = '349158c994084850bfe8711d80d87ce1') #post your token here
access_token <- get_spotify_access_token()

## Loading data
ids <- readRDS("Jonas/SongAndIDV2.rds") #Load data with track IDs
ids <- ids[1:10,]


track_id <- "45QyGXbqTWaFUrIKe2ugs3"

res <- spotifyr::get_track_audio_features(id = track_id)
