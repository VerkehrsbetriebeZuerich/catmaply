#' Heatmap for categorical data using plotly
#'
#' @description \code{catmaply} is used to easily plot categorical data on heatmaps using plotly.
#' It can be used to plot heatmaps on categorical variables or, otherwise, plot continuous variables with categorical color range.
#'
#' @param df data.frame or tibble holding the data.
#' @param x column name holding the axis values for x.
#' @param x_order column name holding the ordering axis values for x. if no order is specified, then x will be used for ordering x; (default:"x").
#' @param x_side on which side the axis labels on the x axis should appear. options: c("top", "bottom"); (default:"top").
#' @param x_tickangle the angle of the axis label on the x axis. options: range -180 until 180; (default:90).
#' @param y column name holding the axis values for y.
#' @param y_order column name holding the ordering axis values for y. if no order is specified, then y will be used for ordering y; (default:"y").
#' @param y_side on which side the axis labels on the y axis should appear. options: c("left", "right"); (default:"left").
#' @param y_tickangle the angle of the axis label on the x axis. options: range -180 until 180; (default:0).
#' @param z column name holding the values for the fields.
#' @param hover_template template to be used to create the hover label; (default:missing).
#' @param hover_hide boolean indicating if the hover label should be hidden or not; (default: FALSE).
#' @param color_palette a color palette vector a function that is able to create one; (default: viridis::plasma).
#' @param categorical_colorbar if the resulting heatmap holds categorical field values or continuous values that belong to a category; (default: FALSE).
#' @param categorical_col if categorical_colorbar is TRUE, then this column is used to create categories; (default: FALSE).
#' @param font_family the typeface that will be applied by the web browser.
#' The web browser will only be able to apply a font if it is available on the system which it operates.
#' Provide multiple font families, separated by commas, to indicate the preference in which to apply fonts if they aren't available on the system;
#' (default: c("Open Sans", "verdana", "arial", "sans-serif")).
#' @param font_size font size to be used for plot. needs to be a number greather than or equal to 1; (default: 12).
#' @param font_color font color to be used for plot; (default: "#444")
#' @param legend boolean indicating if legend should be displayed or not; (default: TRUE).
#' @param legend_col column to be used for legend naming; (default: z/categorical_col)
#' @param legend_interactive whether the legend should be interactive or not; i.e. remove traces on click; (default: T).
#' @param tickformatstops used only if x axis is of type c("POSIXct", "POSIXt"). List of named list where each named list has one or more of the keys listed here: https://plotly.com/r/reference/#heatmap-colorbar-tickformatstops. Default is optimized for summarized data of level day 24 hours;
#' (default:
#' list(
#'   list(dtickrange = list(NULL, 1000), value = "%H:%M:%S.%L ms"),
#'   list(dtickrange = list(1000, 60000), value = "%H:%M:%S s"),
#'   list(dtickrange = list(60000, 3600000), value = "%H:%M m"),
#'   list(dtickrange = list(3600000, 86400000), value = "%H:%M h"),
#'   list(dtickrange = list(86400000, 604800000), value = "%H:%M h"),
#'   list(dtickrange = list(604800000, "M1"), value = "%H:%M h"),
#'   list(dtickrange = list("M1", "M12"), value = "%H:%M h"),
#'   list(dtickrange = list("M12", NULL), value = "%H:%M h")
#'   )
#' )
#' @param source a character string of length 1. Match the value of this string with the source argument in event_data() to retrieve the event data corresponding to a specific plot (shiny apps can have multiple plots).
#'
#' @return plot_ly object
#'
#' @examples
#' \dontrun{
#' library(catmaply)
#' library(viridis)
#' library(dplyr)
#'
#' data("vbz")
#' df <- vbz[[3]]$data
#'
#' # simple plot
#' catmaply(
#'    df,
#'    x='fahrt_seq',
#'    y = "Haltestellenlangname",
#'    y_order = "halt_seq",
#'    z = "Ausl_Kat"
#' )
#'
#'
#' # categorical color range and template
#' catmaply(
#'    df,
#'    x=fahrt_seq,
#'    x_order = fahrt_seq,
#'    x_tickangle = 15,
#'    y = Haltestellenlangname,
#'    y_order = halt_seq,
#'    z = Besetzung,
#'    categorical_colorbar = T,
#'    categorical_col = Ausl_Kat,
#'    color_palette = viridis::inferno,
#'    hover_template = paste(
#'      '<b>Fahrt Nr.</b>:', fahrt_seq,
#'      '<br><b>Haltestelle</b>:', Haltestellenlangname,
#'      '<br><b>Auslastung</b>:', Ausl_Kat,
#'      '<br><b>Besetzung</b>:', round(Besetzung, 2),
#'      '<extra></extra>'
#'    )
#' )
#' }
#'
#' @export
catmaply <- function(
  df,
  x,
  x_order,
  x_side="top",
  x_tickangle=90,
  y,
  y_order,
  y_side="left",
  y_tickangle=0,
  z,
  hover_template,
  hover_hide=F,
  color_palette=viridis::plasma,
  categorical_colorbar=F,
  categorical_col=NA,
  font_family = c("Open Sans", "verdana", "arial", "sans-serif"),
  font_size = 12,
  font_color = "#444",
  legend=T,
  legend_col,
  legend_interactive=T,
  tickformatstops=list(
    list(dtickrange = list(NULL, 1000), value = "%H:%M:%S.%L ms"),
    list(dtickrange = list(1000, 60000), value = "%H:%M:%S s"),
    list(dtickrange = list(60000, 3600000), value = "%H:%M m"),
    list(dtickrange = list(3600000, 86400000), value = "%H:%M h"),
    list(dtickrange = list(86400000, 604800000), value = "%H:%M h"),
    list(dtickrange = list(604800000, "M1"), value = "%H:%M h"),
    list(dtickrange = list("M1", "M12"), value = "%H:%M h"),
    list(dtickrange = list("M12", NULL), value = "%H:%M h")
  ),
  source="catmaply"
) {

  if (!is.data.frame(df))
    stop("Parameter 'df' must be of type data.frame/tibble.")

  # check if categorical_colorbar is logical
  if (!is.logical(categorical_colorbar))
    stop("Parameter 'categorical_colorbar' must be logical")

  # substitute column references, so that they can be passed without quotes
  x <- as.character(substitute(x))
  x_order <- ifelse(missing(x_order), x, as.character(substitute(x_order)))
  y <- as.character(substitute(y))
  y_order <- ifelse(missing(y_order), y, as.character(substitute(y_order)))
  z <- as.character(substitute(z))
  categorical_col <- ifelse(!categorical_colorbar, z, as.character(substitute(categorical_col)))
  legend_col <- ifelse(missing(legend_col), categorical_col, as.character(substitute(legend_col)))

  # check columnnames
  cols <- colnames(df)

  # parameter check / error handling named params
  if (
    !any(is.element(x, cols)) ||
    !any(is.element(x_order, cols)) ||
    !any(is.element(y, cols)) ||
    !any(is.element(y_order, cols)) ||
    !any(is.element(z, cols)) ||
    !any(is.element(categorical_col, cols)) ||
    !any(is.element(legend_col, cols))
  )
    stop("Parameters c('x', 'x_order', 'y', 'y_order', 'z', 'categorical_col', 'legend_col') must be valid column names in df.")

  if (!any(is.element(c("left", "right"), y_side)))
    stop("Parameter 'y_side' only allows the following values: c('left', 'right')")

  if (!any(is.element(c("top", "bottom"), x_side)))
    stop("Parameter 'x_side' only allows the following values: c('top', 'bottom')")

  if (abs(x_tickangle) > 180 || abs(y_tickangle) > 180)
    stop("Parameter 'x_tickangle' and 'y_tickangle' show be in range -180 to 180.")

  if (font_size < 1)
    stop("Parameter 'font_size' needs to be bigger than or equal to one.")

  if (!is.logical(legend))
    stop("Parameter 'legend' needs to be logical/boolean.")

  if (!is.logical(legend_interactive))
    stop("Parameter 'legend_interactive' needs to be logical/boolean.")

  if (!is.logical(hover_hide))
    stop("Parameter 'hover_hide' needs to be logical/boolean.")

  # preprocessing & logic

  # substitute hover_template if submitted; is_hover_template is a workaround
  # missing does not seem to work in a dplyr::mutate function for some reason.
  is_hover_template <- (!missing(hover_template) && !hover_hide)
  if (!missing(hover_template)) {
    hover_template <- substitute(hover_template)
  } else {
    hover_template <- ""
  }

  x_is_time <- F
  # check if x axis is POSXxt
  if ( any(class(df[[x]]) %in% c("POSIXct", "POSIXt")))
    x_is_time <- T

  # check categories and color palette
  cat_col <- unique(stats::na.omit(df[[categorical_col]]))
  cat_leg_comb <- unique(stats::na.omit(df[, c(categorical_col, legend_col)]))

  if (length(cat_col) != NROW(cat_leg_comb))
    stop("You need to define excactly one legend entry per category.")

  # order cat column correctly to resolve issue #12
  ordering <- order(cat_col)
  category_items <- cat_col[ordering]
  legend_items <- cat_leg_comb[[legend_col]][ordering]

  # get color palette
  if (is.function(color_palette)) {
    color_palette <- color_palette(length(cat_col) * ifelse(categorical_colorbar, 2, 1))
  } else if (is.vector(color_palette) && !is.list(color_palette)) {
    color_palette <- color_palette
  } else {
    stop("Parameter 'color_palette' can either be a function producing a color_palette vector or a color paletet vector itself.")
  }

  if (length(color_palette) != (length(category_items) * ifelse(categorical_colorbar, 2, 1))) {
    stop("For each category needs to be exactly one color, if you use a colorbar, then two colors are needed for one category.")
  }

  df <- df %>%
    dplyr::mutate(
      x = !!rlang::sym(x),
      y = !!rlang::sym(y),
      z = !!rlang::sym(z),
      category = !!rlang::sym(categorical_col),
      label =
        dplyr::if_else(
          rep((is_hover_template  && !hover_hide), NROW(df)),
          eval(hover_template),
          paste(
            '<b>x</b>:', x,
            '<br><b>y</b>:', y,
            '<br><b>z</b>:', z,
            '<extra></extra>'
          )
        )
    )
  fig <- plotly::plot_ly(source=source)

  if (legend && legend_interactive) {
    fig <- fig %>%
      add_catmaply_traces(
        df=df,
        x=x,
        y=y,
        z=z,
        hover_hide=hover_hide,
        categorical_colorbar=categorical_colorbar,
        category_items = category_items,
        legend_items=legend_items,
        color_palette=color_palette
      )
  } else {
    fig <- fig %>%
      add_catmaply_single(
        df=df,
        x=x,
        y=y,
        z=z,
        hover_hide=hover_hide,
        categorical_colorbar=categorical_colorbar,
        legend_items=legend_items,
        color_palette=color_palette,
        legend=legend
      )
  }

  #
  if (x_is_time) {
    fig <- fig %>%
      catmaply_time_layout(
        df=df,
        x=x,
        x_side=x_side,
        x_tickangle=x_tickangle,
        y=y,
        y_order=y_order,
        y_side=y_side,
        y_tickangle=y_tickangle,
        tickformatstops=tickformatstops,
        font_family=font_family,
        font_size=font_size,
        font_color=font_color,
        legend=legend
      )
  }
  else {
    fig <- fig %>%
      catmaply_layout(
        df=df,
        x=x,
        x_order=x_order,
        x_side=x_side,
        x_tickangle=x_tickangle,
        y=y,
        y_order=y_order,
        y_side=y_side,
        y_tickangle=y_tickangle,
        font_family=font_family,
        font_size=font_size,
        font_color=font_color,
        legend=legend
      )
  }

  return(fig)
}
