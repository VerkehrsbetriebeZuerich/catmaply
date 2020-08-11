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
#' @param x_range the initial range that should be displayed on the x axis. Only works with non-time x-axis at the moment; (default: 30).
#' @param y column name holding the axis values for y.
#' @param y_order column name holding the ordering axis values for y. if no order is specified, then y will be used for ordering y; (default:"y").
#' @param y_side on which side the axis labels on the y axis should appear. options: c("left", "right"); (default:"left").
#' @param y_tickangle the angle of the axis label on the x axis. options: range -180 until 180; (default:0).
#' @param z column name holding the values for the fields.
#' @param text optional column name holding the values that should be displayed in the fields. NA values will not be displayed.
#' @param text_color font color to be used for text; (default: "#444").
#' @param text_size font size to be used for text/annotation. Needs to be a number greater than or equal to 1; (default: 12).
#' @param text_font_family the typeface that will be applied by the web browser for the text/annotation.
#' The web browser will only be able to apply a font if it is available on the system which it operates.
#' Provide multiple font families, separated by commas, to indicate the preference in which to apply fonts if they aren't available on the system;
#' (default: c("Open Sans", "verdana", "arial", "sans-serif")).
#' @param hover_template template to be used to create the hover label; (default:missing).
#' @param hover_hide boolean indicating if the hover label should be hidden or not; (default: FALSE).
#' @param color_palette a color palette vector a function that is able to create one; (default: viridis::plasma).
#' @param categorical_colorbar if the resulting heatmap holds categorical field values or continuous values that belong to a category; (default: FALSE).
#' @param categorical_col if categorical_colorbar is TRUE, then this column is used to create categories; (default: NA).
#' @param font_family the typeface that will be applied by the web browser.
#' The web browser will only be able to apply a font if it is available on the system which it operates.
#' Provide multiple font families, separated by commas, to indicate the preference in which to apply fonts if they aren't available on the system;
#' (default: c("Open Sans", "verdana", "arial", "sans-serif")).
#' @param font_size font size to be used for plot. needs to be a number greater than or equal to 1; (default: 12).
#' @param font_color font color to be used for plot; (default: "#444").
#' @param legend boolean indicating if legend should be displayed or not; (default: TRUE).
#' @param legend_col column to be used for legend naming; (default: z/categorical_col).
#' @param legend_interactive whether the legend should be interactive or not; i.e. remove traces on click; (default: TRUE).
#' @param tickformatstops used only if x axis is of type c("POSIXct", "POSIXt"). List of named list where each named list has one or
#' more of the keys listed here: https://plotly.com/r/reference/#heatmap-colorbar-tickformatstops. Default is optimized for summarized data of level day 24 hours;
#' (default: \cr
#' list( \cr
#' list(dtickrange = list(NULL, 1000), value = "\%H:\%M:\%S.\%L ms"), \cr
#'   list(dtickrange = list(1000, 60000), value = "\%H:\%M:\%S s"), \cr
#'   list(dtickrange = list(60000, 3600000), value = "\%H:\%M m"), \cr
#'   list(dtickrange = list(3600000, 86400000), value = "\%H:\%M h"), \cr
#'   list(dtickrange = list(86400000, 604800000), value = "\%H:\%M h"), \cr
#'   list(dtickrange = list(604800000, "M1"), value = "\%H:\%M h"), \cr
#'   list(dtickrange = list("M1", "M12"), value = "\%H:\%M h"), \cr
#'   list(dtickrange = list("M12", NULL), value = "\%H:\%M h") \cr
#'   ) \cr
#' ).
#' @param rangeslider boolean value indicating whether the rangeslider should be displayed or not; (default: TRUE).
#' @param slider boolean value indicating whether to use slider or not; if specified, \code{rangeslider} will not be displayed; (default: FALSE).
#' @param slider_steps list holding the configuration of the steps to be created. There are two alternatives: \code{auto} and
#' \code{custom}; whereas the \code{auto} mode creates the steps automatically and \code{custom} takes custom instructions on how to create the steps.
#' For mode \code{auto}, a \code{list} with the following elements has to be submitted (values of the list element are just examples): \cr
#' list( \cr
#'   slider_start=1, \cr
#'   slider_range=15, \cr
#'   slider_shift=5, \cr
#'   slider_step_name="x"
#' ) \cr
#' This will create the steps automatically for you, essentially starting at position \code{slider_start},
#' shifting the window of size \code{slider_range} along the x axis with a stepsize of \code{slider_shift}. The stepnames
#' are automatically selected with the x value of the left side of the slider_range (so for 1 it would take the first value of the x axis as name of the step). \cr
#' With custom, on the other hand, you can define the step configuration without any restrictions. The custom
#' configuration needs to be defined in a \code{list} with the following elements. \cr
#' list( \cr
#'   list(name="Step_One", range=c(1, 50)), \cr
#'   list(name="Step_Two", range=c(5, 55)), \cr
#'   ... \cr
#' ). \cr
#' (default: \cr
#' list( \cr
#'   slider_start=1, \cr
#'   slider_range=15, \cr
#'   slider_shift=5, \cr
#' )).
#' @param slider_currentvalue_prefix prefix to be used for the slider title. Only used if \code{slider=TRUE}. (default: "").
#' @param slider_step_visible boolean indicating if the step names should be displayed for the slider. (default: TRUE).
#' @param slider_currentvalue_visible boolean indicating if the currently selected value should be displayed above the slider. (default: TRUE).
#' @param slider_tick_visible boolean indicating if the tickvalues should be displayed below the slider. (default: TRUE).
#' @param source a character string of length 1. Match the value of this string with the source argument in event_data() to retrieve the event data corresponding to a specific plot (shiny apps can have multiple plots).
#'
#' @return plot_ly object
#'
#' @examples
#' library(catmaply)
#'
#' data("vbz")
#' df <- vbz[[3]]
#'
#' # simple plot
#' catmaply(
#'   df,
#'   x=trip_seq,
#'   x_order = trip_seq,
#'   y = stop_name,
#'   y_order = stop_seq,
#'   z = occ_category
#' )
#'
#'
#' # categorical color range and template
#' catmaply(
#'   df,
#'   x = trip_seq,
#'   y = stop_name,
#'   y_order = stop_seq,
#'   z = occupancy,
#'   categorical_colorbar=TRUE,
#'   categorical_col = occ_category,
#'   hover_template = paste(
#'     '<b>Trip</b>:', trip_seq,
#'     '<br><b>Stop</b>:', stop_seq,
#'     '<br><b>Occupancy</b>:', occ_category,
#'     '<extra></extra>'
#'   )
#' )
#' # for more examples, see vignette
#'
#' @export
catmaply <- function(
  df,
  x,
  x_order,
  x_side="top",
  x_tickangle=90,
  x_range=30,
  y,
  y_order,
  y_side="left",
  y_tickangle=0,
  z,
  text,
  text_color="#444",
  text_size=12,
  text_font_family=c("Open Sans", "verdana", "arial", "sans-serif"),
  hover_template,
  hover_hide=FALSE,
  color_palette=viridis::plasma,
  categorical_colorbar=FALSE,
  categorical_col=NA,
  font_family = c("Open Sans", "verdana", "arial", "sans-serif"),
  font_size = 12,
  font_color="#444",
  legend=TRUE,
  legend_col,
  legend_interactive=TRUE,
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
  rangeslider=TRUE,
  slider=FALSE,
  slider_steps=list(
    slider_start=1,
    slider_range=15,
    slider_shift=5,
    slider_step_name="x"
  ),
  slider_currentvalue_prefix="",
  slider_step_visible=TRUE,
  slider_currentvalue_visible=TRUE,
  slider_tick_visible=TRUE,
  source="catmaply"
) {

  if (!is.data.frame(df))
    stop("Parameter 'df' must be of type data.frame/tibble.")

  # check if categorical_colorbar is logical
  if (!is.logical(categorical_colorbar))
    stop("Parameter 'categorical_colorbar' must be logical")

  # only annotate graph, if text column is provided
  annotated <- !missing(text)

  # substitute column references, so that they can be passed without quotes
  x <- as.character(substitute(x))
  x_order <- ifelse(missing(x_order), x, as.character(substitute(x_order)))
  y <- as.character(substitute(y))
  y_order <- ifelse(missing(y_order), y, as.character(substitute(y_order)))
  z <- as.character(substitute(z))
  categorical_col <- ifelse(!categorical_colorbar, z, as.character(substitute(categorical_col)))
  legend_col <- ifelse(missing(legend_col), categorical_col, as.character(substitute(legend_col)))
  text <- ifelse(!annotated, z, as.character(substitute(text)))

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
    !any(is.element(legend_col, cols)) ||
    !any(is.element(text, cols))
  )
    stop("Parameters c('x', 'x_order', 'y', 'y_order', 'z', 'categorical_col', 'legend_col'. 'text') - if submitted - must be valid column names in df.")

  if (!any(is.element(c("left", "right"), y_side)))
    stop("Parameter 'y_side' only allows the following values: c('left', 'right')")

  if (!any(is.element(c("top", "bottom"), x_side)))
    stop("Parameter 'x_side' only allows the following values: c('top', 'bottom')")

  if (abs(x_tickangle) > 180 || abs(y_tickangle) > 180)
    stop("Parameter 'x_tickangle' and 'y_tickangle' show be in range -180 to 180.")

  if (font_size < 1)
    stop("Parameter 'font_size' needs to be bigger than or equal to one.")

  if (text_size < 1)
    stop("Parameter 'text_size' needs to be bigger than or equal to one.")

  if (!is.logical(legend))
    stop("Parameter 'legend' needs to be logical/boolean.")

  if (!is.logical(legend_interactive))
    stop("Parameter 'legend_interactive' needs to be logical/boolean.")

  if (!is.logical(hover_hide))
    stop("Parameter 'hover_hide' needs to be logical/boolean.")

  if (!is.logical(rangeslider))
    stop("Parameter 'rangeslider' needs to be logical/boolean.")

  if (!is.list(slider_steps))
    stop("Parameter 'slider_steps' needs to be a list.")

  if (!is.numeric(x_range))
    stop("Parameter 'x_range' needs to be integer.")

  if (x_range < 2) {
    warning(paste("Parameter 'x_range' needs to larger than 2.", "Changing parameter from", x_range, "to 2."))
    x_range <- 2
  }

  if (categorical_colorbar && !legend)
    warning("Parameter 'categorical_colorbar' and 'categorical_col' will be ignored if parameter 'legend' is FALSE")

  if (
    !is.logical(slider) ||
    !is.logical(slider_step_visible) ||
    !is.logical(slider_currentvalue_visible) ||
    !is.logical(slider_tick_visible)
  )
    stop("Parameter 'slider', 'slider_step_visible', 'slider_currentvalue_visible', 'slider_tick_visible' need to be logical/boolean.")

  if (!is.character(slider_currentvalue_prefix))
    stop("Parameter 'slider_currentvalue_prefix' needs to be a character.")

  if (!slider_currentvalue_visible && nchar(slider_currentvalue_prefix) > 0) {
    warning(paste("Parameter 'slider_currentvalue_prefix' will be ignored as slider_currentvalue_visible is False"))
    slider_currentvalue_prefix <- ""
  }

  # overrule rangelslider if slider is specified
  #TODO: Test that user cannot activate both
  if (slider && rangeslider) {
    warning(paste("Parameter 'rangeslider' will be ignored as slider is specified"))
    rangeslider <- FALSE
  }

  #TODO: Test that user cannot activate both
  if (slider && legend_interactive) {
    warning(paste("An interactive legend is not supported when using slider at the moment. Overwriting legend_interactive with FALSE."))
    legend_interactive <- FALSE
  }

  if (slider)
    x_range <- c()
  else
    x_range <- c(0.5, x_range + 0.5)

  # preprocessing & logic

  # substitute hover_template if submitted; is_hover_template is a workaround
  # missing does not seem to work in a dplyr::mutate function for some reason.
  is_hover_template <- (!missing(hover_template) && !hover_hide)
  if (!missing(hover_template)) {
    hover_template <- substitute(hover_template)
  } else {
    hover_template <- ""
  }

  x_is_time <- FALSE
  # check if x axis is POSXxt
  if ( any(class(df[[x]]) %in% c("POSIXct", "POSIXt"))){
    x_is_time <- TRUE
  }

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
  pal_len <- length(cat_col) * ifelse(categorical_colorbar, 2, 1)
  if (is.function(color_palette)) {
    color_palette <- color_palette(pal_len)
  } else if (is.vector(color_palette) && !is.list(color_palette)) {
    color_palette <- utils::head(color_palette, pal_len)
  } else {
    stop("Parameter 'color_palette' can either be a function producing a color_palette vector or a color paletet vector itself.")
  }

  if (length(color_palette) != pal_len) {
    stop("For each category needs to be exactly one color, if you use a colorbar, then two colors are needed for one category.")
  }

  # create strucutre for following plots
  # changes to this structure might affect traces
  df <- df %>%
    dplyr::mutate(
      x = !!rlang::sym(x),
      y = !!rlang::sym(y),
      z = !!rlang::sym(z),
      text = !!rlang::sym(text),
      x_order = !!rlang::sym(x_order),
      category = !!rlang::sym(categorical_col),
      legend = !!rlang::sym(legend_col),
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

  if (slider) { # slider - special handling for annotations
    fig <- fig %>%
      add_catmaply_slider(
        df = df,
        annotated=annotated,
        slider_currentvalue_prefix=slider_currentvalue_prefix,
        slider_steps=slider_steps,
        slider_step_visible=slider_step_visible,
        slider_currentvalue_visible=slider_currentvalue_visible,
        slider_tick_visible=slider_tick_visible,
        hover_hide=hover_hide,
        text_color=text_color,
        text_size=text_size,
        text_font_family=text_font_family,
        color_palette=color_palette,
        categorical_colorbar=categorical_colorbar,
        category_items=category_items,
        legend_items=legend_items,
        legend=legend
      )

  } else { # no slider
    if (legend && legend_interactive) {
      fig <- fig %>%
        add_catmaply_traces(
          df=df,
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
          hover_hide=hover_hide,
          categorical_colorbar=categorical_colorbar,
          legend_items=legend_items,
          color_palette=color_palette,
          legend=legend
        )
    }

    if ( annotated ) { # annotated
      fig <- fig %>%
        plotly::layout(
          annotations = catmaply_annotations(
            df=df,
            annotated=annotated,
            text_color=text_color,
            text_size=text_size,
            text_font_family=text_font_family
          )
        )
    }

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
        legend=legend,
        rangeslider=rangeslider
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
        x_range=x_range,
        y=y,
        y_order=y_order,
        y_side=y_side,
        y_tickangle=y_tickangle,
        font_family=font_family,
        font_size=font_size,
        font_color=font_color,
        legend=legend,
        rangeslider=rangeslider
      )
  }

  return(fig)
}
