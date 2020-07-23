#' Add Catmaply traces
#'
#' Function to produce catmaply traces.
#'
#' @param fig plotly object
#' @param df data.frame or tibble holding the data.
#' @param hover_hide boolean indicating if the hover label should be hidden or not; (default: FALSE).
#' @param color_palette a color palette vector.
#' @param categorical_colorbar if the resulting heatmap holds categorical field values or continuous values that belong to a category; (default: FALSE).
#' @param categorical_col if categorical_colorbar is TRUE, then this column is used to create categories; (default: FALSE).
#' @param category_items disctinct/unique items of ordered category items
#' @param legend_items distinct/unique items of ordered legend items
#'
#' @return plot_ly object
#'
#'
#' @keywords internal
#' @export
add_catmaply_traces <- function(
  fig,
  df,
  hover_hide,
  color_palette,
  categorical_colorbar,
  category_items,
  legend_items
) {

  for (i in seq.int(length.out = length(category_items))) {

    temp <- df %>%
      dplyr::mutate(
        z = ifelse(.data$category == category_items[i], .data$z, NA),
      )

    if (categorical_colorbar) {
      index <- ((i - 1) * 2) + 1
      colorscale <- array(
        data=c(0, 1, color_palette[index], color_palette[index + 1]),
        dim= c(2,2)
      )
    } else {
      colorscale <- array(
        data=c(0, 1, rep(color_palette[i], 2)),
        dim= c(2,2)
      )
    }

    if (!hover_hide) {
      # show hover label
      fig <- fig %>%
        plotly::add_trace(
          type = "heatmap",
          name = legend_items[i],
          data = temp,
          x = ~x,
          y = ~y,
          z = ~z,
          text = ~label,
          hovertemplate = '%{text}',
          colorscale=colorscale,
          showlegend=T,
          showscale=F,
          legendgroup = legend_items[i]
        )
    } else {
      # don't show hover label
      fig <- fig %>%
        plotly::add_trace(
          type = "heatmap",
          name = legend_items[i],
          data = temp,
          x = ~x,
          y = ~y,
          z = ~z,
          hoverinfo= "skip",
          colorscale=colorscale,
          showlegend=T,
          showscale=F,
          legendgroup = legend_items[i]
        )
    }

  }

  return(fig)
}

#' Add single Catmaply traces
#'
#' Function to produce a single catmaply trace without interactive legend.
#'
#' @param fig plotly object
#' @param df data.frame or tibble holding the data.
#' @param color_palette a color palette vector.
#' @param legend_items distinct/unique items of ordered legend items
#' @param legend boolean indicating if legend should be displayed or not; (default: TRUE).
#' @param colorbar_y y position of colorbar; (default: NA).
#'
#' @return plot_ly object
#'
#' @keywords internal
#' @export
add_catmaply_single <- function(
  fig,
  df,
  hover_hide,
  color_palette,
  categorical_colorbar,
  legend_items,
  legend,
  visible=1,
  colorbar_y=NA
) {

  discrete_col <- discrete_coloring(
    categories=legend_items,
    col_palette=color_palette,
    range_min = min(stats::na.omit(df$z)),
    range_max = max(stats::na.omit(df$z))
  )

  if (legend) { # legend

    # legend & hover
    if (!hover_hide) {
      fig <- fig %>%
        plotly::add_trace(
          type = "heatmap",
          name = "",
          data = df,
          x = ~x,
          y = ~y,
          z = ~z,
          text = ~label,
          hovertemplate = '%{text}',
          visible = visible,
          showlegend=FALSE,
          colorscale=discrete_col$colorscale,
          colorbar=list(
            title="",
            len=1,
            tickvals=discrete_col$tickvals,
            ticktext=discrete_col$ticktext,
            y=colorbar_y
          )
        )
    } else { # legend & no hover
      fig <- fig %>%
        plotly::add_trace(
          type = "heatmap",
          name = "",
          data = df,
          x = ~x,
          y = ~y,
          z = ~z,
          text = ~label,
          hoverinfo= "skip",
          visible = visible,
          showlegend=FALSE,
          colorscale=discrete_col$colorscale,
          colorbar=list(
            title="",
            len=1,
            tickvals=discrete_col$tickvals,
            ticktext=discrete_col$ticktext,
            y=colorbar_y
          )
        )
    }
  } else { # no legend


    if (!hover_hide) { # no legend & hover
      fig <- fig %>%
        plotly::add_trace(
          type = "heatmap",
          name = "",
          data = df,
          x = ~x,
          y = ~y,
          z = ~z,
          text = ~label,
          hovertemplate = '%{text}',
          colorscale=discrete_col$colorscale,
          visible = visible,
          showscale=F
        )
    } else { # no legend & no hover
      fig <- fig %>%
        plotly::add_trace(
          type = "heatmap",
          name = "",
          data = df,
          x = ~x,
          y = ~y,
          z = ~z,
          text = ~label,
          hoverinfo= "skip",
          colorscale=discrete_col$colorscale,
          visible = visible,
          showscale=F
        )
    }

  }
  #browser()

  return(fig)
}


#' Add catmaply slider traces
#'
#' Function to produce catmaply traces.
#'
#' @param fig plotly object
#' @param df data.frame or tibble holding the data.
#' @param annotated boolean indicating if annotations should be displayed.
#' @param text_color font color to be used for text; (default: "#444").
#' @param text_size font size to be used for text/annotation. Needs to be a number greather than or equal to 1; (default: 12).
#' @param text_font_color the typeface that will be applied by the web browser for the text/annotation.
#' The web browser will only be able to apply a font if it is available on the system which it operates.
#' Provide multiple font families, separated by commas, to indicate the preference in which to apply fonts if they aren't available on the system;
#' (default: c("Open Sans", "verdana", "arial", "sans-serif")).
#' @param slider_steps list holding the configuration of the steps to be created. There are two alternatives: \code{auto} and
#' \code{custom}; whereas the \code{auto} mode creates the steps automatically and \code{custom} takes custom instructions on how to create the steps.
#' For mode \code{auto}, a \code{list} with the following elements has to be submitted (values of the list element are just examples): \cr
#' list( \cr
#'   slider_start=1, \cr
#'   slider_range=15, \cr
#'   slider_shift=5, \cr
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
#' @param hover_hide boolean indicating if the hover label should be hidden or not; (default: FALSE).
#' @param color_palette a color palette vector.
#' @param categorical_colorbar if the resulting heatmap holds categorical field values or continuous values that belong to a category; (default: FALSE).
#' @param categorical_col if categorical_colorbar is TRUE, then this column is used to create categories; (default: FALSE).
#' @param category_items disctinct/unique items of ordered category items
#' @param legend_items distinct/unique items of ordered legend items
#'
#' @return plot_ly object
#'
#'
#' @keywords internal
#' @export
add_catmaply_slider <- function(
  fig,
  df,
  annotated,
  text_color="#444",
  text_size=12,
  text_font_family=c("Open Sans", "verdana", "arial", "sans-serif"),
  slider_steps,
  slider_currentvalue_prefix="",
  slider_step_visible,
  slider_currentvalue_visible,
  slider_tick_visible,
  hover_hide,
  color_palette,
  categorical_colorbar,
  category_items,
  legend_items,
  legend
) {

  visible_index <- 1

  if (all(c("slider_start", "slider_range", "slider_shift", "slider_step_name") %in% names(slider_steps))){

    step_name_col <- slider_steps$slider_step_name[1]

    if (!(step_name_col %in% colnames(df)))
      stop("Element 'slider_step_name' in list 'slider_steps' must be a valid reference to a column in df.")

    # check x and step name column are unique
    x_unqiue <- unique(stats::na.omit(df[["x"]]))
    x_step_unique <- unique(stats::na.omit(df[, c("x", step_name_col)]))

    if (length(x_unqiue) != NROW(x_step_unique))
      stop("You need to define excactly one stepname entry per values on the x axis.")

    # get range to calculate number of steps and to get step names
    x_range <- unique(df[['x_order']])
    x <- unique(df[[step_name_col]])[order(x_range)]
    x_range <- x_range[order(x_range)]

    # create slider steps automatically; mode -> auto
    slider_start <- slider_steps[["slider_start"]]
    slider_range <- slider_steps[["slider_range"]]
    slider_shift <- slider_steps[["slider_shift"]]

    iterations <- ceiling((length(x_range) - (slider_start - 1) - slider_range) / slider_shift)

    slider_steps <- vector(mode = "list", length = length(iterations))

    for ( i in seq.int(iterations) ) {
      slider_steps[[i]] <- list(name=as.character(x[slider_start]), range=c(slider_start, slider_range))
      slider_start <- slider_start + slider_shift
      slider_range <- slider_range + slider_shift
    }

  } else if (!all(sapply(slider_steps, function(step) all(c("name", "range") %in% names(step))))) {
    # no auto mode and custom config does not show correct elements.
    stop("Parameter 'slider_steps' must either have the necessary elements for mode auto or custom. Please check parameter 'slider_steps' function documentation for more info.")
  }

  steps <- vector(mode = "list", length = length(slider_steps))

  start_annotations <- NA

  for (i in seq.int(from = 1, to = length(slider_steps), by = 1)) {

    lower_bound <- slider_steps[[i]]$range[1]
    upper_bound <- slider_steps[[i]]$range[2]

    if (lower_bound >= upper_bound)
      stop(paste("Trying to build slider, however, lower bound is higher or equal than upper bound for step:", slider_steps[[i]]$name))

    tmp <- dplyr::filter(df, dplyr::between(df[["x_order"]], lower_bound, upper_bound))

    # get the indexes of the legend items relevant to the current trace
    legend_idx <- which(legend_items %in% unique(tmp[['legend']]))
    # get the indexes of the color palette; remember to handle categorical colorbar
    if (!categorical_colorbar) {
      color_palette_idx <- legend_idx
    } else {
      color_palette_idx <- c(sapply(legend_idx, function(i) c(2 * (i - 1) + 1, 2 * (i - 1) + 2)))
    }

    # get catmaply single trace for this range
    fig <- fig %>%
      add_catmaply_single(
        df=tmp,
        hover_hide=hover_hide,
        color_palette=color_palette[color_palette_idx],
        categorical_colorbar=categorical_colorbar,
        legend_items=legend_items[legend_idx],
        legend=legend,
        visible=i==visible_index,
        colorbar_y=1
      )


    annotations <- list()

    if ( annotated )
      annotations <- catmaply_annotations(
        df=tmp,
        annotated=annotated,
        text_color=text_color,
        text_size=text_size,
        text_font_family=text_font_family
      )

    if (i == visible_index)
      start_annotations <- annotations

    steps[[i]] = list(
      args = list(
        # 'visible',
        list(visible=1:length(slider_steps) == i),
        list(annotations=annotations)
      ),
      method = 'update',
      label = ifelse(slider_step_visible, slider_steps[[i]]$name, "")
    )
  }

  fig <- fig %>%
    plotly::layout(
      showlegend=F,
      annotations = start_annotations,
      sliders=list(
        list(
          active = 0,
          currentvalue = list(
            visible=slider_currentvalue_visible,
            prefix = slider_currentvalue_prefix
          ),
          steps = steps,
          ticklen=ifelse(slider_tick_visible, 7, 0),
          minorticklen=ifelse(slider_tick_visible, 4, 0)
        )
      )
    )

  return(fig)
}


#' Get catmaply annotation list
#'
#' Function to produce catmaply traces.
#'
#' @param fig plotly object
#' @param df data.frame or tibble holding the data.
#' @param annotated boolean indicating if annotations should be displayed.
#' @param text_color font color to be used for text; (default: "#444").
#' @param text_size font size to be used for text/annotation. Needs to be a number greather than or equal to 1; (default: 12).
#' @param text_font_color the typeface that will be applied by the web browser for the text/annotation.
#' The web browser will only be able to apply a font if it is available on the system which it operates.
#' Provide multiple font families, separated by commas, to indicate the preference in which to apply fonts if they aren't available on the system;
#' (default: c("Open Sans", "verdana", "arial", "sans-serif")).
#'
#' @return list
#'
#'
#' @keywords internal
#' @export
catmaply_annotations <- function(
  df,
  annotated,
  text_color="#444",
  text_size=12,
  text_font_family=c("Open Sans", "verdana", "arial", "sans-serif")
) {
  df <- df %>%
    stats::na.omit()

  annotations <- list()

  if (NROW(df) > 0) {
    annotations <- lapply(
      1:NROW(df),
      function(i) {
        list(x = df$x[i],
             y=df$y[i],
             text=as.character(df$text[i]),
             showarrow=F,
             font=list(
               family=text_font_family,
               color=text_color,
               size=text_size
             )
        )
      })
  }

  return(annotations)
}
