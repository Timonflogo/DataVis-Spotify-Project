## Libraries
pacman::p_load("jsonlite"
               , "dplyr"
               , "tidyr"
               , "spotifyr"
               , "pbapply"
               , "tibble"
)

Sys.setenv(SPOTIFY_CLIENT_ID = 'a0299cfb25944ecdbffc9079b987ff9a')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f773ae310efa4f74a751bb6f50d4b0b4')
access_token <- get_spotify_access_token()

#loading the data
track_features <- readRDS("R_dataframe/get_track_features.RDS")

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
# saveRDS(total_artist_list, file = "DataVisualizationApp/Data/artist_genre.rds")

# load data
artist_genre <- readRDS("DataVisualizationApp/Data/artist_genre.rds")

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

genre_split <- data.frame(str_split_fixed(genre_df$genre
                                          ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X1 %>% 
  str_replace(pattern = 'c\\(\\\"'
              , replacement = "") %>% 
  str_replace(pattern = '\\\"'
              , replacement = "") %>% 
  as.data.frame() %>% 
  rename('genre' = ".")

genre_split[genre_split == "0"] <- 'No genre found for selected artist'
# 
# genre_split2 <- data.frame(str_split_fixed(genre_df$genre
#                                           ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X2 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre2' = ".")
# genre_split2[genre_split2 == ""] <- 'No genre found for selected artist'
# 
# genre_split3 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X3 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre3' = ".")
# genre_split3[genre_split3 == ""] <- 'No genre found for selected artist'
# 
# genre_split4 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X4 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre4' = ".")
# genre_split4[genre_split4 == ""] <- 'No genre found for selected artist'
# 
# genre_split5 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X5 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre5' = ".")
# genre_split5[genre_split5 == ""] <- 'No genre found for selected artist'
# 
# genre_split6 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X6 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre6' = ".")
# genre_split6[genre_split6 == ""] <- 'No genre found for selected artist'
# 
# genre_split7 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X7 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre7' = ".")
# genre_split7[genre_split7 == ""] <- 'No genre found for selected artist'
# 
# genre_split8 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X8 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre8' = ".")
# genre_split8[genre_split8 == ""] <- 'No genre found for selected artist'
# 
# genre_split9 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X9 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre9' = ".")
# genre_split9[genre_split9 == ""] <- 'No genre found for selected artist'
# 
# genre_split10 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X10 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre10' = ".")
# genre_split10[genre_split10 == ""] <- 'No genre found for selected artist'
# 
# genre_split11 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X11 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre11' = ".")
# genre_split11[genre_split11 == ""] <- 'No genre found for selected artist'
# 
# genre_split12 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X12 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre12' = ".")
# genre_split12[genre_split12 == ""] <- 'No genre found for selected artist'
# 
# genre_split13 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X13 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre13' = ".")
# genre_split13[genre_split13 == ""] <- 'No genre found for selected artist'
# 
# genre_split14 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X14 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre14' = ".")
# genre_split14[genre_split14 == ""] <- 'No genre found for selected artist'
# 
# genre_split15 <- data.frame(str_split_fixed(genre_df$genre
#                                            ,",", n = max(sapply(strsplit(as.character(genre_df$genre),','),length))))$X15 %>% 
#   str_replace(pattern = 'c\\(\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\\"'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\"\\)'
#               , replacement = "") %>% 
#   str_replace(pattern = '\\)'
#               , replacement = "") %>% 
#   as.data.frame() %>% 
#   rename('genre15' = ".")
# genre_split15[genre_split15 == ""] <- 'No genre found for selected artist'

artist_genre_split <- cbind(as.data.frame(genre_df$artist_id)
                            , genre_split
                            # , genre_split2
                            # , genre_split3
                            # , genre_split4
                            # , genre_split5
                            # , genre_split6
                            # , genre_split7
                            # , genre_split8
                            # , genre_split9
                            # , genre_split10
                            # , genre_split11
                            # , genre_split12
                            # , genre_split13
                            # , genre_split14
                            # , genre_split15
                            )

artist_genre_split <- artist_genre_split %>% 
  # gather(key = "artist_id", value = "genre", -1) %>% 
  rename(artist_id = "genre_df$artist_id")


# saveRDS(artist_genre_split, file = "DataVisualizationApp/Data/artist_genre_split.rds")

# Joining streams with artist genres
stream <- readRDS("DataVisualizationApp/Data/stream_selected_c_clean.rds")
artist_genre <- readRDS("DataVisualizationApp/Data/artist_genre_split.rds")

stream_full_features_genres <- stream %>% 
  inner_join(artist_genre, by = "artist_id")

# stream_full_features_genres <- stream_full_features %>%
#   gather(key = genre
#          , value
#          ,-(1:22)) %>% 
#   filter(value != 0) 



# saveRDS(artist_genre_split, file = "DataVisualizationApp/Data/stream_full_features_genres.rds")

