library(tidyverse)
library(plotly)


#' Generate Test Data
#'
#' @param nr_stops 
#'
#' @return tibble
#' @export
generate_test_data <- function(
  nr_stops=35,
  nr_drives=100
){
  # ---------------------------
  # generate data
  drive_ids <- sort(rep(seq(1, nr_drives), nr_stops))
  drive_names <- paste("Drive", drive_ids, sep = "_")
  
  stop_ids <- rep(seq.int(1, nr_stops), nr_drives)
  stop_names <- paste("Stop", stop_ids, sep = "_")
  
  nr_passengers = sample(1:50, nr_stops * nr_drives, replace=TRUE)
  
  # ---------------------------
  # create input dataframe
  df <- tibble(
    "Stop_id" = stop_ids,
    "Stop_names" = stop_names,
    "Drive_id" = drive_ids,
    "Drive_names" = drive_names,
    "Passengers" = nr_passengers,
    "Category" = ifelse(nr_passengers > 35, "high", ifelse(nr_passengers > 20, "medium", "low")),
    "Some_label" = rep("<b>Bla Label</b>", nr_stops * nr_drives)
  )
  
  return(df)
}


