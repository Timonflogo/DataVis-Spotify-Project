
#Replace _ with space and capitalize first letter
cap_space <- function(string){
  pacman::p_load(stringr,Hmisc)
  capitalize(str_replace(string,pattern = "_",replacement = " "))
}

cap_space_radar <- function(string){
  pacman::p_load(stringr,Hmisc)
  capitalize(str_replace(string,pattern = "_exposed",replacement = " "))
}