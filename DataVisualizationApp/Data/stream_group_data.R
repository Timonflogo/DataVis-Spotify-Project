
df <- readRDS("Data/stream_selected_c_clean.rds")
stream_group_date <- df %>%
  melt(which(names(dataInput1) == "track_id"):which(names(dataInput1) == "duration_ms")) %>%
  group_by(date, month, variable) %>%
  summarise(value = sum(value))
stream_group_date$index <- sort(rep(seq(1,nrow(stream_group_date)/length(unique(stream_group_date$variable))),9)) #Create an index to map the y values onto a continous x axis
