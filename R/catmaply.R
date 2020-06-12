#' catmaply
#'
#' @param df df
#' @param x x
#' @param x_order x_order
#' @param y y
#' @param y_order y_order
#' @param vals vals
#' @param color_palette color_palette
#' @param categorical_plot categorical_plot
#' @param x_side x_side
#' @param y_side y_side
#'
#' @return catmaply
#' @export
catmaply<- function(
  df,
  x,
  x_order,
  y,
  y_order,
  vals,
  color_palette=viridis::plasma,
  categorical_plot=T,
  x_side="top",
  y_side="left"
) {

  # check columnnames
  cols <- colnames(df)

  if (
    !any(is.element(x, cols)) ||
    !any(is.element(x_order, cols)) ||
    !any(is.element(y, cols)) ||
    !any(is.element(y_order, cols)) ||
    !any(is.element(vals, cols))
  )
    stop("Parameters c('x', 'x_order', 'y', 'y_order', 'vals) must be valid column names in df.")

  # parameter check / error handling named params
  if (!categorical_plot)
    stop("This function only allows categorical heatmaps for the moment.")

  if (!any(is.element(c("left", "right"), y_side)))
    stop("Parameter 'y_side' only allows the following values: c('left', 'right')")

  if (!any(is.element(c("top", "bottom"), x_side)))
    stop("Parameter 'x_side' only allows the following values: c('top', 'bottom')")

  # check categories and color palette
  if (categorical_plot) {
    cat_col <- unique(stats::na.omit(df[[vals]]))

    # get color palette
    if (is.function(color_palette)) {
      col_pal <- color_palette(length(cat_col))
    } else if (is.vector(color_palette) && !is.list(color_palette)) {
      col_pal <- color_palette
    } else {
      stop("Parameter 'color_palette' can either be a function producing a color_palette vector or already a vector.")
    }

    if (!identical(length(col_pal), length(cat_col))) {
      stop("For each category needs to be exactly one category.")
    }
  } # to be implemented

  # aus_kat <- unique(stats::na.omit(df$Ausl_Kat))
  # col_palette <- viridis::plasma(length(aus_kat))

  fig <- plotly::plot_ly()

  for (i in seq.int(length.out = length(cat_col))) {

    temp <- df %>%
      dplyr::mutate(
        x = !!rlang::sym(x),
        y = !!rlang::sym(y),
        vals = ifelse(!!rlang::sym(vals) == cat_col[i], !!rlang::sym(vals), NA),
        label =
          ifelse(
            !is.na(!!rlang::sym('Besetzung')),
            paste(
              '<b>Drive</b>:', 'FZ_AB',
              '<br><b>Stop</b>:', 'Haltestellenlangname',
              '<br><b>Nr Passengers</b>:', 'Besetzung',
              '<extra>A. K.', 'Ausl_Kat', '</extra>'
            ),
            paste(
              '<b>Drive</b>:', 'FZ_AB',
              '<br><b>Stop</b>:', 'Haltestellenlangname',
              '<br><b>Nr Passengers</b>:N/A',
              '<extra>A. K.', 'Ausl_Kat', '</extra>'
            )
          )
      )

    colorscale <- array(
      data=c(0, 1, rep(col_palette[i], 2)),
      dim= c(2,2)
    )

    fig <- fig %>%
      plotly::add_trace(
        type = "heatmap",
        name = paste("A. K.", cat_col[i]),
        data = temp,
        x = ~x,
        y = ~y,
        z = ~vals,
        text = ~label,
        hovertemplate = '%{text}',
        # paste(
        #   '<b>Drive</b>: %{x}',
        #   '<br><b>Stop</b>: %{y}',
        #   '<br><b>Nr Passengers</b>: %{z}',
        #   '<br>%{text}'
        # ),
        colorscale=colorscale,
        showlegend=T,
        showscale=F,
        legendgroup = paste("A. K.", cat_col[i])
      )
    # %>%
    #   add_annotations(
    #     x = a_k$fahrt_seq[which(!is.na(a_k$Besetzung))],
    #     y = a_k$Haltestellenlangname[which(!is.na(a_k$Besetzung))],
    #     text = round(a_k$Besetzung[which(!is.na(a_k$Besetzung))], 0),
    #     showarrow = FALSE,
    #     ax = 20,
    #     ay = -20
    #   )

  }

  fig <- fig %>%
    plotly::layout(
      showlegend=T,
      xaxis = list(
        title="",
        tickmode='linear',
        range = c(0,30),
        tickangle = 90,
        categoryorder="array",
        categoryarray=unique(df[[x]][order(as.numeric(df[[x_order]]))]),
        side = x_side,
        # rangeselector = list(
        #   buttons = list(
        #     list(
        #       count = "8",
        #       label = "6 - 8",
        #       step = "hour",
        #       stepmode = "backward"
        #     )
        #   )
        # ),
        rangeslider = list(visible=TRUE)
      ),
      yaxis = list(
        title="",
        side="left",
        fixedrange = TRUE,
        categoryorder="array",
        categoryarray=unique(df[[y]][order(df[[y_order]])])
      )
    )

  return(plotly::partial_bundle(fig))

}
