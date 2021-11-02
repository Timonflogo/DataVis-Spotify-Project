# Load libraries 
pacman::p_load(dplyr, lubridate, hms)

# summary statistics

TotalTimePlayed <- function(dataInput1) {
  total_time_played <- round(seconds_to_period(sum(dataInput1$msPlayed) / 1000), 0)
}
