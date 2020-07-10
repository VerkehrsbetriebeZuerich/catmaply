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
