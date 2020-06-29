#' Heatmap for categorical data using plotly
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
#' @param legend_col column to be used for legend naming; (default: z/color_palette)
#' @param source a character string of length 1. Match the value of this string with the source argument in event_data() to retrieve the event data corresponding to a specific plot (shiny apps can have multiple plots).
#'
#' @return catmaply object
#' @export
catmaply<- function(
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
  source="catmaply"
) {

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

  # parameter check / error handling named params

  # substitute hover_template if submitted; is_hover_template is a workaround
  # missing does not seem to work in a dplyr::mutate function for some reason.
  is_hover_template <- (!missing(hover_template) && !hover_hide)
  if (!missing(hover_template)) {
    hover_template <- substitute(hover_template)
  } else {
    hover_template <- ""
  }

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

  if (!is.logical(hover_hide))
    stop("Parameter 'hover_hide' needs to be logical/boolean.")

  # check categories and color palette
  cat_col <- unique(stats::na.omit(df[[categorical_col]]))
  cat_leg_comb <- unique(stats::na.omit(df[, c(categorical_col, legend_col)]))

  if (length(cat_col) != NROW(cat_leg_comb))
    stop("You need to define excactly one legend entry per category.")

  # order cat column correctly to resolve issue #12
  cat_col <- cat_col[order(cat_col)]
  leg_col <- cat_leg_comb[[legend_col]][order(cat_col)]

  # get color palette
  if (is.function(color_palette)) {
    col_pal <- color_palette(length(cat_col) * ifelse(categorical_colorbar, 2, 1))
  } else if (is.vector(color_palette) && !is.list(color_palette)) {
    col_pal <- color_palette
  } else {
    stop("Parameter 'color_palette' can either be a function producing a color_palette vector or a color paletet vector itself.")
  }

  if (length(col_pal) != (length(cat_col) * ifelse(categorical_colorbar, 2, 1))) {
    stop("For each category needs to be exactly one color, if you use a colorbar, then two colors are needed for one category.")
  }

  fig <- plotly::plot_ly(source=source)

  for (i in seq.int(length.out = length(cat_col))) {

    temp <- df %>%
      dplyr::mutate(
        x = !!rlang::sym(x),
        y = !!rlang::sym(y),
        z = ifelse(!!rlang::sym(categorical_col) == cat_col[i], !!rlang::sym(z), NA),
        label =
          dplyr::if_else(
            rep((is_hover_template  && !hover_hide), NROW(.)),
            eval(hover_template),
            paste(
              '<b>x</b>:', x,
              '<br><b>y</b>:', y,
              '<br><b>z</b>:', z
            )
          )
      )

    if (categorical_colorbar) {
      index <- ((i - 1) * 2) + 1
      colorscale <- array(
        data=c(0, 1, col_pal[index], col_pal[index + 1]),
        dim= c(2,2)
      )
    } else {
      colorscale <- array(
        data=c(0, 1, rep(col_pal[i], 2)),
        dim= c(2,2)
      )
    }

    if (!hover_hide) {
      # show hover label
      fig <- fig %>%
        plotly::add_trace(
          type = "heatmap",
          name = leg_col[i],
          data = temp,
          x = ~x,
          y = ~y,
          z = ~z,
          text = ~label,
          hovertemplate = '%{text}',
          colorscale=colorscale,
          showlegend=T,
          showscale=F,
          legendgroup = leg_col[i]
        )
    } else {
      # don't show hover label
      fig <- fig %>%
        plotly::add_trace(
          type = "heatmap",
          name = leg_col[i],
          data = temp,
          x = ~x,
          y = ~y,
          z = ~z,
          hoverinfo= "skip",
          colorscale=colorscale,
          showlegend=T,
          showscale=F,
          legendgroup = leg_col[i]
        )
    }

  }

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

  return(fig)
}

