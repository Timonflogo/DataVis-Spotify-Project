

initializeEnvironmentAndGetData <-
  function(clientID,
           ClientSecret,
           GetIDs = FALSE,
           GetTrackFeatures = FALSE,
           GetArtistGenre = FALSE,
           DoDataPreproccesing = TRUE) {
    
    #Load libraries ----
    pacman::p_load(
      "jsonlite","dplyr","tidyr","spotifyr","pbapply","stringr","data.table","lubridate","tibble"
    )
    
    Sys.setenv(SPOTIFY_CLIENT_ID = clientID)
    Sys.setenv(SPOTIFY_CLIENT_SECRET = ClientSecret)
    access_token <- get_spotify_access_token()
    
    ## Loading streaming history ----
    address <- "RawStreamHistData/StreamingHistory"
    file_type <- ".json"
    
    paths <- list()
    NoStreamHistfiles <- length(list.files("RawStreamHistData/")[list.files("RawStreamHistData/") %like% "StreamingHistory"]) #find the number of streaming history files
    for (i in seq(1, NoStreamHistfiles)) {
      paths[[i]] <- paste(address, i - 1, file_type, sep = "")
    }
    
    message("Loaded streaming history paths")
    
    message("Loading streaming history into environment")
    
    list_df <- list()
    df <- data.frame()
    for (i in seq(from = 1, to = length(paths))) {
      print(paths[[i]])
      list_df[[i]] <- jsonlite::fromJSON(txt = readLines(paths[[i]]), flatten = TRUE)
    }
    df <- do.call(rbind, list_df)
    
    message("Creating track artist key")
    
    ## Concatenating track and artist
    df <- df %>%
      mutate(track_artist = paste(df$trackName, df$artistName, sep = " "))
    
    ## Unique track and song combinations
    df_unique <- data.frame(track_artist = unique(df$track_artist))
    
    ## Get IDs ----
    
    if (GetIDs) {
      message("GetIDs = TRUE, hence artist id, artist name and track id is being downloaded.")
      
      message("Collecting track information through search query function.")
      
      f_artist_id <- function(track_artist) {
        if (length(spotifyr::search_spotify(
          q = track_artist,
          type = "track",
          limit = 1
        )) != 0) {
          spotifyr::search_spotify(q = track_artist,
                                   type = "track",
                                   limit = 1)[[1]][[1]]$id
        } else {
          NULL
        }
      }
      
      f_artist_name <- function(track_artist) {
        if (length(spotifyr::search_spotify(
          q = track_artist,
          type = "track",
          limit = 1
        )) != 0) {
          spotifyr::search_spotify(q = track_artist,
                                   type = "track",
                                   limit = 1)[[1]][[1]]$name
        } else {
          NULL
        }
      }
      
      f_track_id <- function(track_artist) {
        if (length(spotifyr::search_spotify(
          q = track_artist,
          type = "track",
          limit = 1
        )) != 0) {
          spotifyr::search_spotify(q = track_artist,
                                   type = "track",
                                   limit = 1)$id
        } else {
          NULL
        }
      }
      
      #running the functions
      message("Getting artist id")
      system.time({
        df_unique$artist_id <-
          pblapply(X = df_unique$track_artist, FUN = f_artist_id)
      }, gcFirst = TRUE)
      saveRDS(df_unique, file = "Data/artist_id.rds")
      
      message("Getting artist name")
      system.time({
        df_unique$artist_name <-
          pblapply(X = df_unique$track_artist, FUN = f_artist_name)
      }, gcFirst = TRUE)
      saveRDS(df_unique, file = "Data/artist_id.rds")
      
      message("Getting track id")
      system.time({
        df_unique$track_id <-
          pblapply(X = df_unique$track_artist, FUN = f_track_id)
      }, gcFirst = TRUE)
      
      saveRDS(df_unique, file = "Data/artist_id.rds")
    } else {
      message("GetIDs = FALSE, hence the artist id, artist name and track ids will not be retrieved.")
    }
    
    ## Get track features ----
    
    if (GetTrackFeatures) {
      joined_df <- readRDS("Data/artist_id.rds")
      
      # function for extracting all features of each song in the list based on the id
      f_track_feature <- function(track_feature) {
        if (length(spotifyr::get_track_audio_features(id = track_feature)) != 0) {
          spotifyr::get_track_audio_features(id = track_feature)
        } else {
          NULL
        }
      }
      
      # empty dataframe to save the track features
      df_track_features <- data.frame()
      
      # Running the function and saving it into the dataframe
      message("Collecting track features")
      system.time({
        df_track_features <-
          pbsapply(X = joined_df$track_id, FUN = f_track_feature)
      }, gcFirst = TRUE)
      
      joined_df <- joined_df[joined_df$track_id != 'NULL',]
      
      df_track_feature_temp <- data.frame()
      for (i in seq(from = 1, to = length(df_track_features))) {
        #Control if the track has 18 features
        if (length(df_track_features[[i]]) != 18) {
          print(paste0("skip track: ", i))
          next
        }
        df_track_feature_temp <-
          rbind(df_track_feature_temp, df_track_features[[i]])
      }
      
      df_track_features <- df_track_feature_temp
      rm(df_track_feature_temp)
      
      # Rename the column id to track_id for easier join
      df_track_features <- df_track_features %>%
        as.data.frame() %>% # specify as a dataframe because t() creates a matrix
        rename('track_id' = 'id') #rename the column for later join
      
      # Joining the track features table with the main joined table
      main_df <- joined_df %>%
        filter(track_id != 'NULL') %>% 
        select(-c(track_artist,artist_id)) %>% 
        mutate(track_id = unlist(joined_df$track_id)) %>% #Unlist track_id to make it a vector that can be used for the join
        left_join(df_track_features
                  , by = 'track_id'
                  )
      
      saveRDS(main_df, file = "Data/get_track_features.rds")
      
    }
    
    ## Get artist genre ----
    
    if (GetArtistGenre){
      
      message("Starting to Get Artist Genre....")
      
      #loading the data
      track_features <- readRDS("Data/get_track_features.rds")
      
      temp_artist <- str_split(string = track_features$artist_id
                               , pattern = ","
                               , n = 2
                               , simplify = TRUE) %>% #splitting artist_id into 2 columns - main artist and the rest
        as.data.frame() %>% 
        select(1) %>% # selecting the first artist
        rename('artist_id' = "V1") %>% # Renaming the column back to artist_id
        unlist() %>% # unlist in order to use str_replace 
        # Cleaning the artist_id from 'c' and brackets
        str_replace(pattern = 'c\\("' 
                    , replacement = "") %>% 
        str_replace(pattern = '"'
                    , replacement = "") %>% 
        as.data.frame() %>% # Setting it back into a dataframe
        rename('artist_id' = ".") #renaming it back to artist_id
      
      # getting unique artist_id to get the genres of each artist
      unique_artist_id <- temp_artist %>% 
        select(artist_id) %>% 
        unlist() %>% 
        as.character() %>% 
        unique()
      
      # getting all characteristics to each artist including the genre
      system.time({total_artist_list <- pbsapply(X = unique_artist_id,FUN = get_artists)}, gcFirst = TRUE)
      
      total_artist_list <- lapply(seq_len(ncol(total_artist_list)), function(i) total_artist_list[,i])
      
      message("Function get_artist() is succesfully run. Saving total artists list.")
      
      saveRDS(total_artist_list, file = "Data/artist_genre.rds")
      
      # load data
      artist_genre <- readRDS("Data/artist_genre.rds")
      
      # empty dataframe to store artist id and their genres
      genre_df <- data.frame()
      genre_df <- genre_df %>% 
        add_column(artist_id = NA
                   , genre = NA)
      
      # Loop to create a dataframe of artist id and their genres
      for (i in seq(1, length(artist_genre))) {
        print(i)
        # If artist genre is empty skip
        if (!is.na(artist_genre[[i]]) == TRUE) {
          print(i)
          genre_df[i, 1] <- unlist(artist_genre[[i]]["id"][[1]])
          
          # If genre does not exist for this track, put 0 into the cell
          if (length(paste(as.data.frame(artist_genre[[i]][["genres"]][[1]]), sep = "")) == 0) {
            genre_df[i, 2] <- 0
          } else{
            genre_df[i, 2] <-
              paste(as.data.frame(artist_genre[[i]][["genres"]][[1]]), sep = "")
          }
          next
        }
      }
      
      ### Genre_splits ----
      
      genre_split1 <- data.frame(str_split_fixed(genre_df$genre
                                                 ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X1 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre1' = ".")
      
      genre_split1[genre_split1 == ""] <- 0
      
      genre_split2 <- data.frame(str_split_fixed(genre_df$genre
                                                 ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X2 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre2' = ".")
      genre_split2[genre_split2 == ""] <- 0
      
      genre_split3 <- data.frame(str_split_fixed(genre_df$genre
                                                 ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X3 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre3' = ".")
      genre_split3[genre_split3 == ""] <- 0
      
      genre_split4 <- data.frame(str_split_fixed(genre_df$genre
                                                 ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X4 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre4' = ".")
      genre_split4[genre_split4 == ""] <- 0
      
      genre_split5 <- data.frame(str_split_fixed(genre_df$genre
                                                 ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X5 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre5' = ".")
      genre_split5[genre_split5 == ""] <- 0
      
      genre_split6 <- data.frame(str_split_fixed(genre_df$genre
                                                 ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X6 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre6' = ".")
      genre_split6[genre_split6 == ""] <- 0
      
      genre_split7 <- data.frame(str_split_fixed(genre_df$genre
                                                 ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X7 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre7' = ".")
      genre_split7[genre_split7 == ""] <- 0
      
      genre_split8 <- data.frame(str_split_fixed(genre_df$genre
                                                 ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X8 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre8' = ".")
      genre_split8[genre_split8 == ""] <- 0
      
      genre_split9 <- data.frame(str_split_fixed(genre_df$genre
                                                 ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X9 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre9' = ".")
      genre_split9[genre_split9 == ""] <- 0
      
      genre_split10 <- data.frame(str_split_fixed(genre_df$genre
                                                  ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X10 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre10' = ".")
      genre_split10[genre_split10 == ""] <- 0
      
      genre_split11 <- data.frame(str_split_fixed(genre_df$genre
                                                  ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X11 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre11' = ".")
      genre_split11[genre_split11 == ""] <- 0
      
      genre_split12 <- data.frame(str_split_fixed(genre_df$genre
                                                  ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X12 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre12' = ".")
      genre_split12[genre_split12 == ""] <- 0
      
      genre_split13 <- data.frame(str_split_fixed(genre_df$genre
                                                  ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X13 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre13' = ".")
      genre_split13[genre_split13 == ""] <- 0
      
      genre_split14 <- data.frame(str_split_fixed(genre_df$genre
                                                  ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X14 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre14' = ".")
      genre_split14[genre_split14 == ""] <- 0
      
      genre_split15 <- data.frame(str_split_fixed(genre_df$genre
                                                  ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X15 %>% 
        str_replace(pattern = 'c\\(\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\\"'
                    , replacement = "") %>% 
        str_replace(pattern = '\\"\\)'
                    , replacement = "") %>% 
        str_replace(pattern = '\\)'
                    , replacement = "") %>% 
        as.data.frame() %>% 
        rename('genre15' = ".")
      genre_split15[genre_split15 == ""] <- 0
      
      artist_genre_split <- cbind(as.data.frame(genre_df$artist_id)
                                  , genre_split1
                                  , genre_split2
                                  , genre_split3
                                  , genre_split4
                                  , genre_split5
                                  , genre_split6
                                  , genre_split7
                                  , genre_split8
                                  , genre_split9
                                  , genre_split10
                                  , genre_split11
                                  , genre_split12
                                  , genre_split13
                                  , genre_split14
                                  , genre_split15)
      
      ### Artist_genre_split dataframe ----
      
      artist_genre_split <- artist_genre_split %>% 
        # gather(key = "artist_id", value = "genre", -1) %>% 
        rename(artist_id = "genre_df$artist_id")
      
      saveRDS(artist_genre_split, file = "Data/artist_genre_split.rds")
      
      # Joining streams with artist genres
      stream <- readRDS("Data/stream_selected_c_clean.rds")
      artist_genre <- readRDS("Data/artist_genre_split.rds")
      
      stream_full_features <- stream %>% 
        inner_join(artist_genre, by = "artist_id")
      
      stream_full_features_genres <- stream_full_features %>%
        gather(key = genre
               , value
               ,-(1:22)) %>% 
        filter(value != 0) 
      
    } else {
      message("Artist genre will not be downloaded.")
    }
    
    
    ## Joining track features onto stream history ----
    
    if (DoDataPreproccesing) {
      message("Starting to merge track features onto the streaming history.....")
      
      ## Loading features of unique songs that the user listened to
      track_features <- readRDS('Data/get_track_features.rds')
      
      df_unique <- readRDS("Data/artist_id.rds") %>% 
        select(-c("artist_name"))
      df_unique <- df_unique[df_unique$track_id != 'NULL',]
      df_unique$track_id <- unlist(df_unique$track_id)
      
      track_features <- track_features %>%
        left_join(df_unique,by = "track_id")
      
      ## Joining features on stream history
      stream_full_features <- df %>%
        inner_join(track_features, by = "track_artist")
      
      # filter out songs having ms played longer than ms duration
      stream_full_features <-
        stream_full_features[stream_full_features$msPlayed != 0, ]
      stream_full_features <-
        stream_full_features[stream_full_features$msPlayed <= stream_full_features$duration_ms, ]
      
      # splitting artist_id into first id and the rest
      temp_artist <- str_split(
        string = stream_full_features$artist_id,pattern = ",",n = 2,
        simplify = TRUE) %>% #splitting artist_id into 2 columns - main artist and the rest
        as.data.frame() %>%
        select(1) %>% # selecting the first artist
        rename('artist_id' = "V1") %>% # Renaming the column back to artist_id
        unlist() %>% # unlist in order to use str_replace
        # Cleaning the artist_id from 'c' and brackets
        str_replace(pattern = 'c\\("'
                    , replacement = "") %>%
        str_replace(pattern = '"'
                    , replacement = "") %>%
        as.data.frame() %>% # Setting it back into a dataframe
        rename('artist_id' = ".") #renaming it back to artist_id
      
      # Replacing artist_id in stream_full_features with temp_artist
      stream_full_features <- stream_full_features %>%
        mutate(artist_id = temp_artist$artist_id)
      
      # Selecting columns to work with since some are irrelevant
      stream_selected_c <- stream_full_features %>%
        select(artist_id,artistName,track_id,trackName,msPlayed,endTime,duration_ms,danceability,energy,loudness,speechiness:tempo) %>%
        # standardizing features into range of 0 - 1
        mutate_at(scales::rescale, .vars = vars(danceability:tempo))
      
      # Calculating the features per ms to have a single unit for aggregation
      stream_selected_c <-
        setDT(stream_selected_c)[, paste0(names(stream_selected_c)[which(names(stream_selected_c)== "danceability"):which(names(stream_selected_c)== "tempo")]
                                          , "_per_ms") := lapply(.SD, `/`, stream_selected_c$duration_ms)
                                 , .SDcols = danceability:tempo]
      
      # Calculating the features value based on how long the track was actually played
      stream_selected_c <-
        setDT(stream_selected_c)[, paste0(names(stream_selected_c)[which(names(stream_selected_c)== "danceability"):which(names(stream_selected_c)== "tempo")]
                                          , "_exposed") := lapply(.SD, `*`, stream_selected_c$msPlayed)
                                 , .SDcols = danceability_per_ms:tempo_per_ms]
      
      # replacing inf values with 0
      stream_selected_c <- stream_selected_c %>%
        mutate_all(function(x)
          ifelse(is.infinite(x), 0, x))
      
      # Creating date and time columns from endTime
      stream_selected_c <- stream_selected_c %>%
        mutate(date = as.Date(endTime),
               time = format(as.POSIXct(endTime
                                        , format = "%Y-%m-%d %H:%M")
                             , "%H:%M:%S")) %>%
        mutate(weekday = weekdays(date)) %>% # extracting weekdays form the date
        mutate(year_week = as.numeric(strftime(date, format = "%Y%W")) + 1) %>%
        mutate(start_day_week = floor_date(date, unit = "week", week_start = 1)) %>%
        mutate(month = months(start_day_week)) %>%
        relocate(c(date
                   , month
                   , year_week
                   , weekday
                   , start_day_week
                   , time) #change the position of these 2 columns
                 ,
                 .before = artist_id) %>%
        relocate(c(track_id,artist_id)
                 , .before = trackName) %>%
        select(-c(danceability:tempo_per_ms))
      
      saveRDS(stream_selected_c, file = "Data/stream_selected_c_clean.rds")
      
      message("The function is succesfully run.")
      
      message(paste0("You have a total streaming history of ", nrow(df)," streams. This consists of ",length(unique(stream_selected_c$track_id))," tracks among, ",length(unique(stream_selected_c$artist_id))," artists."))
      
    }
  }
