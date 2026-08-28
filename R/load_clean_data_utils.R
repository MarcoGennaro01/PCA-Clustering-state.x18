library(usa)
library(tidyverse)


#' Loads facts data from the USA package
get_data <- function(){
  data("state_facts")
  df <- as.data.frame(state_facts)
  return(df)
}

#' Cleans data
#' Renames columns, scales data, selects everything but name, admission and vote
#' drops rows with missing values
#' returns clean DF
clean_data <- function(df){

  rownames(df) <- df$name
  df <- dplyr::select(df,-c(name, admission)) |>
  drop_na() |>
  scale()

  return(df)
}