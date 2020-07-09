#' Generates the parameters necessary for dicrete coloring and colorbar
#'
#' @param categories categories, for coloring should done
#' @param col_palette the color palette
#'
#' @return list(colorscale, tickvals, ticktext)
#'
#' @importFrom utils tail
#'
#' @keywords internal
#'
#' @export
discrete_coloring <- function(categories, col_palette) {

  bvals <- c(0, seq.int(length.out = length(categories)))
  pal <- col_palette

  if (length(bvals) != (length(pal) + 1)) {
    stop("length(bvals) should be equal to (length(pal) -1)")
  }

  bvals <- bvals[order(bvals)]
  nvals <- (bvals - min(bvals)) / (max(bvals) - min(bvals))

  dcolorscale <- array(NA, dim = c((length(nvals) * 2) -2, 2))

  for (i in seq.int(length.out = length(nvals) -1 )) {
    index <- ((i - 1) * 2) + 1
    dcolorscale[index,] <- c(nvals[i], pal[i])
    dcolorscale[index + 1,] <- c(nvals[i + 1], pal[i])
  }

  # calculate tick values for legend (lowest point to max point)
  # works only with even spacing until now
  ticks <- seq.int(from = 1, to = max(bvals) * 2, by = 1)
  # calc percentage of ticks * range (max - min) + min
  tick_vals <- (
    ticks[ticks %% 2 != 0] / max(ticks)
  ) * (
    max(bvals) - min(utils::tail(bvals, -1))
  ) + min(utils::tail(bvals, -1))

  tick_text <- categories

  return(
    list(
      colorscale=dcolorscale,
      tickvals=tick_vals,
      ticktext=tick_text
    )
  )
}



#  #' Generate Test Data
#  #'
#  #' @param nr_stops number of stops
#  #' @param nr_drives number of drives
#  #'
#  #' @return tibble
#  #'
#  #' @export
#  generate_test_data <- function(
#    nr_stops=35,
#    nr_drives=100
#  ){
#    # ---------------------------
#    # generate data
#    drive_ids <- sort(rep(seq(1, nr_drives), nr_stops))
#    drive_names <- paste("Drive", drive_ids, sep = "_")
#
#    stop_ids <- rep(seq.int(1, nr_stops), nr_drives)
#    stop_names <- paste("Stop", stop_ids, sep = "_")
#
#    nr_passengers = sample(1:50, nr_stops * nr_drives, replace=TRUE)
#
#    # ---------------------------
#    # create input dataframe
#    df <- dplyr::tibble(
#      "Stop_id" = stop_ids,
#      "Stop_names" = stop_names,
#      "Drive_id" = drive_ids,
#      "Drive_names" = drive_names,
#      "Passengers" = nr_passengers,
#      "Category" = ifelse(nr_passengers > 35, "high", ifelse(nr_passengers > 20, "medium", "low")),
#      "Some_label" = rep("<b>Bla Label</b>", nr_stops * nr_drives)
#    )
#
#    return(df)
#  }

