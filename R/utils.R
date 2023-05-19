#' Generates the parameters necessary for discrete coloring and colorbar
#'
#' @param df catmaply tibble
#' @param col_palette the color palette
#'
#' @return list(colorscale, tickvals, ticktext)
#'
#' @importFrom utils tail
#'
#' @keywords internal
discrete_coloring <- function(df, color_palette) {

  if (!is.vector(col_palette) || is.list(col_palette))
    stop("Parameter 'col_palette' must be a vector.")

  discrete_colorbar <- FALSE
  if ((length(unique(df$category)) * 2) == length(col_palette)) {
    discrete_colorbar <- TRUE
  } else if (length(categories) == length(col_palette)) {
    exp_col_palette <- color_palette
    for (col in col_palette) exp_col_palette <- c(exp_col_palette, col, col)
    color_palette <- exp_col_palette
  } else {
    stop("Parameter 'col_palette' must have the same or twice the length of category parameter.")
  }

  # calculate bounds of colorbar
  bounds <- df %>%
    dplyr::group_by(category) %>%
    dplyr::summarise(
      cat_bound_min = min(z),
      cat_bound_max = max(z),
      cat_tickval = mean(c(min(z), max(z)))
    ) %>%
    tidyr::pivot_longer(
      cols = starts_with("cat_"),
      names_to = "var_name",
      values_to = "var_value",
    ) %>%
    dplyr::mutate(
      normalized_value = (var_value - min(var_value))/ (max(var_value) - min(var_value))
    ) %>%
    dplyr::arrange(-desc(category), -desc(var_value))

  # calculate bounds of categories
  dcolorscale <- bounds %>%
    dplyr::filter(substr(var_name, 1, 9) == "cat_bound") %>%
    dplyr::mutate(color = color_palette) %>%
    dplyr::select(normalized_value, color) %>%
    as.matrix()
  colnames(dcolorscale) <- NULL

  # fill gaps between categories with empty/white space
  n_row = NROW(dcolorscale)
  filled_dcolorscale = matrix(nrow=1, ncol = 2)
  filled_dcolorscale[1,] <- dcolorscale[1,]

  artificial_offset = 0
  for(i in seq.int(2, n_row)){
    categroy_gap <- ifelse(i%%2 == 0 && i != n_row, as.double(dcolorscale[i+1,1]) - as.double(dcolorscale[i,1]), 0)
    if( i %% 2 != 0 || i == n_row || categroy_gap == 0) {
      temp_mat = matrix(nrow=1, ncol = 2)
      temp_mat[1,] = c(as.double(dcolorscale[i,1])-artificial_offset, dcolorscale[i,2])
      filled_dcolorscale <- rbind(filled_dcolorscale, temp_mat)
      artificial_offset <- 0
    } else {
      artificial_offset = ifelse(categroy_gap > .000002, .000001, categroy_gap / 3)
      temp_mat = matrix(nrow=3, ncol = 2)
      temp_mat[1,] = c(as.double(dcolorscale[i,1])+artificial_offset, dcolorscale[i,2])
      temp_mat[2,] = c(as.double(dcolorscale[i,1])+artificial_offset, "#FFFFFFFF")
      temp_mat[3,] = c(as.double(dcolorscale[i+1,1])-artificial_offset, "#FFFFFFFF")
      filled_dcolorscale <- rbind(filled_dcolorscale, temp_mat)
    }
  }

  # get tick values
  tick_vals <- bounds %>%
    dplyr::filter(var_name == "cat_tickval") %>%
    .$var_value

  # get tick text
  tick_text <- df %>%
    dplyr::select(category, legend) %>%
    dplyr::distinct() %>%
    dplyr::arrange(-desc(category)) %>%
    .$legend

  return(
    list(
      colorscale=new_mat,
      tickvals=tick_vals,
      ticktext=as.character(tick_text)
    )
  )
}
