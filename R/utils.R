#' Generates the parameters necessary for dicrete coloring and colorbar
#'
#' @param categories categories, for coloring should done
#' @param col_palette the color palette
#' @param range_min min of z range; (default: 1)
#' @param range_min max of z range; (default: length(categories))
#'
#' @return list(colorscale, tickvals, ticktext)
#'
#' @importFrom utils tail
#'
#' @keywords internal
#'
#' @export
discrete_coloring <- function(categories, col_palette, range_min, range_max) {

  if (!is.vector(categories))
    stop("Parameter 'categories' must be a vector.")

  if (!is.vector(col_palette))
    stop("Parameter 'col_palette' must be a vector.")

  discrete_colorbar <- F
  if ((length(categories) * 2) == length(col_palette)) {
    discrete_colorbar <- T
  } else if (length(categories) != length(col_palette)) {
    stop("Parameter 'col_palette' must have the same of twice the length of category parameter.")
  }

  bvals <- c(0, seq.int(length.out = length(categories)))

  bvals <- bvals[order(bvals)]
  nvals <- (bvals - min(bvals)) / (max(bvals) - min(bvals))

  dcolorscale <- array(NA, dim = c((length(nvals) * 2) -2, 2))

  for (i in seq.int(length.out = length(nvals) -1 )) {
    index <- ((i - 1) * 2) + 1
    dcolorscale[index,] <- c(nvals[i], ifelse(discrete_colorbar, col_palette[index], col_palette[i]))
    dcolorscale[index + 1,] <- c(nvals[i + 1], ifelse(discrete_colorbar, col_palette[index + 1], col_palette[i]))
  }

  # calculate tick values for legend (lowest point to max point)
  # works only with even spacing until now
  ticks <- seq.int(from = 1, to = max(bvals) * 2, by = 1)
  range_min <- ifelse(discrete_colorbar, range_min, 1)
  range_max <- ifelse(discrete_colorbar, range_max, max(bvals))
  # calc percentage of ticks * range (max - min) + min
  tick_vals <- (
    ticks[ticks %% 2 != 0] / max(ticks)
  ) * (
    range_max - range_min
  ) + range_min

  tick_text <- categories

  return(
    list(
      colorscale=dcolorscale,
      tickvals=tick_vals,
      ticktext=tick_text
    )
  )
}
