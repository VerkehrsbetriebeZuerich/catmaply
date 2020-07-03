#' Adds time layout to catmaply object
#'
#' This layout is used if the x axis is of type time.
#'
#' @param fig plotly object
#' @param df data.frame or tibble holding the data.
#' @param x column name holding the axis values for x.
#' @param x_side on which side the axis labels on the x axis should appear. options: c("top", "bottom"); (default:"top").
#' @param x_tickangle the angle of the axis label on the x axis. options: range -180 until 180; (default:90).
#' @param y column name holding the axis values for y.
#' @param y_order column name holding the ordering axis values for y. if no order is specified, then y will be used for ordering y; (default:"y").
#' @param y_side on which side the axis labels on the y axis should appear. options: c("left", "right"); (default:"left").
#' @param y_tickangle the angle of the axis label on the x axis. options: range -180 until 180; (default:0).
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
#' @param font_family the typeface that will be applied by the web browser.
#' The web browser will only be able to apply a font if it is available on the system which it operates.
#' Provide multiple font families, separated by commas, to indicate the preference in which to apply fonts if they aren't available on the system;
#' (default: c("Open Sans", "verdana", "arial", "sans-serif")).
#' @param font_size font size to be used for plot. needs to be a number greather than or equal to 1; (default: 12).
#' @param font_color font color to be used for plot; (default: "#444")
#' @param legend boolean indicating if legend should be displayed or not; (default: TRUE).
#'
#' @return plot_ly object
#'
#' @keywords internal
#' @export
catmaply_time_layout <- function(
  fig,
  df,
  x,
  x_side,
  x_tickangle,
  y,
  y_order,
  y_side,
  y_tickangle,
  tickformatstops,
  font_family,
  font_size,
  font_color,
  legend
) {
  fig <- fig %>%
    plotly::layout(
      showlegend=legend,
      xaxis = list(
        title="",
        side = x_side,
        type='date',
        tickangle = 90,
        tickformatstops = tickformatstops,
        rangeslider = list(visible=TRUE)
      ),
      yaxis = list(
        title="",
        side=y_side,
        tickangle=y_tickangle,
        fixedrange = TRUE,
        categoryorder="array",
        categoryarray=unique(df[[y]][order(df[[y_order]])])
      ),
      font = list(
        family = font_family,
        size = font_size,
        color = font_color
      )
    )
  return(fig)
}


#' Adds time layout to catmaply object
#'
#' This layout is used if the x axis is of type time.
#'
#' @param fig plotly object
#' @param df data.frame or tibble holding the data.
#' @param x column name holding the axis values for x.
#' @param x_order column name holding the ordering axis values for x. if no order is specified, then x will be used for ordering x; (default:"x").
#' @param x_side on which side the axis labels on the x axis should appear. options: c("top", "bottom"); (default:"top").
#' @param x_tickangle the angle of the axis label on the x axis. options: range -180 until 180; (default:90).
#' @param y column name holding the axis values for y.
#' @param y_order column name holding the ordering axis values for y. if no order is specified, then y will be used for ordering y; (default:"y").
#' @param y_side on which side the axis labels on the y axis should appear. options: c("left", "right"); (default:"left").
#' @param y_tickangle the angle of the axis label on the x axis. options: range -180 until 180; (default:0).
#' @param font_family the typeface that will be applied by the web browser.
#' The web browser will only be able to apply a font if it is available on the system which it operates.
#' Provide multiple font families, separated by commas, to indicate the preference in which to apply fonts if they aren't available on the system;
#' (default: c("Open Sans", "verdana", "arial", "sans-serif")).
#' @param font_size font size to be used for plot. needs to be a number greather than or equal to 1; (default: 12).
#' @param font_color font color to be used for plot; (default: "#444")
#' @param legend boolean indicating if legend should be displayed or not; (default: TRUE).
#'
#' @return plot_ly object
#'
#' @keywords internal
#' @export
catmaply_layout <- function(
  fig,
  df,
  x,
  x_order,
  x_side,
  x_tickangle,
  y,
  y_order,
  y_side,
  y_tickangle,
  font_family,
  font_size,
  font_color,
  legend
){

  fig <- fig %>%
    plotly::layout(
      showlegend=legend,
      xaxis = list(
        title="",
        tickmode='linear',
        range = c(0,30),
        tickangle = x_tickangle,
        categoryorder="array",
        categoryarray=unique(df[[x]][order(as.numeric(df[[x_order]]))]),
        side = x_side,
        rangeslider = list(visible=TRUE)
      ),
      yaxis = list(
        title="",
        side=y_side,
        tickangle=y_tickangle,
        fixedrange = TRUE,
        categoryorder="array",
        categoryarray=unique(df[[y]][order(df[[y_order]])])
      ),
      font = list(
        family = font_family,
        size = font_size,
        color = font_color
      )
    )
}
