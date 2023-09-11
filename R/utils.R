#' Generates the parameters necessary for discrete coloring and colorbar
#'
#' @param df catmaply tibble
#' @param col_palette the color palette
#' @param categorical_color_range whether to use categorical color range or not.
#'
#' @return list(colorscale, tickvals, ticktext)
#'
#' @importFrom utils tail
#'
#' @keywords internal
discrete_coloring <- function(df, color_palette, categorical_color_range) {

  if (!is.data.frame(df))
    stop("Parameter 'df' must be a data.frame or tibble.")

  if (!all(c("x", "y", "z", "category", "legend") %in% colnames(df)))
    stop(
      paste(
        "DataFrame/Tibble 'df' must have at least the following columns:",
        "x", "y", "z", "category", "legend"
      )
    )

  if (!is.vector(color_palette) || is.list(color_palette))
    stop("Parameter 'color_palette' must be a vector.")

  discrete_colorbar <- FALSE
  if ((length(unique(df$category)) * 2) == length(color_palette)) {
    discrete_colorbar <- TRUE
  } else if (length(unique(df$category)) == length(color_palette)) {
    exp_col_palette <- c()
    for (col in color_palette) exp_col_palette <- c(exp_col_palette, col, col)
    color_palette <- exp_col_palette
  } else {
    stop(
      paste(
        "Parameter 'color_palette' must have the same or twice ",
        "the length of vcategory parameter."
      )
    )
  }

  # calculate bounds of colorbar
  bounds <- df %>%
    dplyr::group_by(.data$category) %>%
    dplyr::summarise(
      cat_bound_min = min(.data$z),
      cat_bound_max = max(.data$z),
      cat_tickval = mean(c(min(.data$z), max(.data$z))),
      cat_range = max(.data$z) - min(.data$z)
    ) %>%
    dplyr::arrange(-dplyr::desc(.data$cat_bound_max))


  for (row_nr in seq.int(length.out = nrow(bounds)-1)) {
    if (bounds[row_nr, ]$cat_bound_max > bounds[row_nr + 1, ]$cat_bound_min) {
      stop(
        paste(
          "Overlapping categories detected, this leads to incorrect vidualizations;",
          "categories affected:\n",
          paste(capture.output(print(bounds[c(row_nr, row_nr + 1), 1:3])), collapse = "\n")
        )
      )
    }
  }

  # unpivot
  bounds <- bounds %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("cat_"),
      names_to = "var_name",
      values_to = "var_value",
    )

  # check min range:
  range_check <- bounds %>%
    dplyr::filter(.data$var_name == "cat_range" & .data$var_value == 0)

  if (nrow(range_check) > 0 & categorical_color_range) {
    warning(
      paste(
        "Category with range 0 has been found; a categorical color",
        "range for a category with range 0 will result very narrow",
        "lines in the legend. Please check and verify the following",
        "categories :\n",
        paste(capture.output(print(range_check)), collapse = "\n")
      )
    )
  }

  # calculate bounds of categories using normalized bounds and color palette
  dcolorscale <- bounds %>%
    dplyr::filter(substr(.data$var_name, 1, 9) == "cat_bound") %>%
    dplyr::mutate(
      normalized_value =
        (.data$var_value - min(.data$var_value)) /
        (max(.data$var_value) - min(.data$var_value))
    ) %>%
    dplyr::arrange(-dplyr::desc(.data$category), -dplyr::desc(.data$var_value)) %>%
    dplyr::mutate(color = color_palette) %>%
    dplyr::select(.data$normalized_value, .data$color) %>%
    as.matrix()
  colnames(dcolorscale) <- NULL

  # fill gaps between categories with empty/white space
  n_row = NROW(dcolorscale)
  filled_dcolorscale = matrix(nrow=1, ncol = 2)
  filled_dcolorscale[1,] <- dcolorscale[1,]

  artificial_offset = 0
  for(i in seq.int(2, n_row)){
    categroy_gap <- ifelse(
      i%%2 == 0 && i != n_row,
      as.double(dcolorscale[i+1,1]) - as.double(dcolorscale[i,1]),
      0
    )
    if( i %% 2 != 0 || i == n_row || categroy_gap == 0) {
      temp_mat = matrix(nrow=1, ncol = 2)
      temp_mat[1,] = c(as.double(dcolorscale[i,1])-artificial_offset, dcolorscale[i,2])
      filled_dcolorscale <- rbind(filled_dcolorscale, temp_mat)
      artificial_offset <- 0
    } else {
      artificial_offset = ifelse(categroy_gap > .002, .001, categroy_gap / 3)
      temp_mat = matrix(nrow=3, ncol = 2)
      temp_mat[1,] = c(as.double(dcolorscale[i,1])+artificial_offset, dcolorscale[i,2])
      temp_mat[2,] = c(as.double(dcolorscale[i,1])+artificial_offset, "#FFFFFFFF")
      temp_mat[3,] = c(as.double(dcolorscale[i+1,1])-artificial_offset, "#FFFFFFFF")
      filled_dcolorscale <- rbind(filled_dcolorscale, temp_mat)
    }
  }

  # get tick values
  tick_vals <- bounds %>%
    dplyr::filter(.data$var_name == "cat_tickval")
  tick_vals <-tick_vals$var_value

  # get tick text
  tick_text <- df %>%
    dplyr::select(.data$category, .data$legend) %>%
    dplyr::distinct() %>%
    dplyr::arrange(-dplyr::desc(.data$category))
  tick_text <- tick_text$legend

  return(
    list(
      colorscale=filled_dcolorscale,
      tickvals=tick_vals,
      ticktext=as.character(tick_text)
    )
  )
}
