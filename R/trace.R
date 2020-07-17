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
  legend
) {

  if (legend) {
    discrete_col <- discrete_coloring(
      categories=legend_items,
      col_palette=color_palette,
      range_min = min(stats::na.omit(df$z)),
      range_max = max(stats::na.omit(df$z))
    )

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
        showlegend=FALSE,
        colorscale=discrete_col$colorscale,
        colorbar=list(
          title="",
          #len=0.5,
          tickvals=discrete_col$tickvals,
          ticktext=discrete_col$ticktext
        )
      )
  } else {
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
        showscale=F
      )
  }

  return(fig)
}


#' Add catmaply slider traces
#'
#' Function to produce catmaply traces.
#'
#' @param fig plotly object
#' @param df data.frame or tibble holding the data.
#' @param text_color font color to be used for text; (default: "#444").
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
  text_color="#444",
  slider_prefix="",
  slider_steps,
  hover_hide=NA,
  color_palette,
  categorical_colorbar,
  category_items,
  legend_items
) {

  visible_index <- 1

  #TODO: Test that user cannot activate both
  if (all(c("slider_start", "slider_range", "slider_shift") %in% names(slider_steps))){

    x_range <- unique(df[['x_order']])
    x <- unique(df[['x']])[order(x_range)]
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

    discrete_col <- discrete_coloring(
      categories=legend_items[legend_idx],
      col_palette=color_palette[color_palette_idx],
      range_min = min(stats::na.omit(tmp$z)),
      range_max = max(stats::na.omit(tmp$z))
    )

    fig <- fig %>%
      plotly::add_trace(
        type = "heatmap",
        data = tmp,
        x = ~x,
        y = ~y,
        z = ~z,
        text = ~label,
        hovertemplate = '%{text}',
        visible = ifelse(i == visible_index, T, F),
        showlegend = FALSE,
        colorscale=discrete_col$colorscale,
        colorbar=list(
          title="",
          len=1,
          tickvals=discrete_col$tickvals,
          ticktext=discrete_col$ticktext,
          y=1
        )
      )

    tmp <- tmp %>%
      stats::na.omit()

    annotations <- lapply(
      1:NROW(tmp),
      function(i) {
        list(x = tmp$x[i],
             y=tmp$y[i],
             text=as.character(tmp$text[i]),
             showarrow=F,
             font=list(
               color=text_color
             )
        )
      })

    if (i == visible_index)
      start_annotations <- annotations

    steps[[i]] = list(
      args = list(
        # 'visible',
        list(visible=1:iterations == i),
        list(annotations=annotations)
      ),
      method = 'update',
      label = slider_steps[[i]]$name
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
            prefix = slider_prefix
          ),
          steps = steps
        )
      )
    )

  return(fig)
}
