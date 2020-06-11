
#' Bt Demo Data
#'
#' @description to be removed
#'
#' @param input_data input_data from generate_demo_data
#'
#' @return plotly object
#'
#' @importFrom magrittr "%>%"
#'
#' @export
bt_demo <- function(
  input_data
) {
  df <- input_data %>%
    dplyr::arrange(!!rlang::sym("Drive_id"), !!rlang::sym('Stop_id'))

  nr_drives <- length(unique(df$Drive_id))
  nr_stops <- length(unique(df$Stop_id))

  # ---------------------------
  # create poc plot
  fig <-
    plotly::plot_ly() %>%
    plotly::add_trace(
      type = "heatmap",
      name = "",
      data = df,
      x = ~Drive_names,
      y = ~Stop_names,
      z = ~Passengers,
      text = ~Some_label,
      hovertemplate = paste('<b>Drive</b>: %{x}',
                            '<br><b>Stop</b>: %{y}',
                            '<br><b>Nr Passengers</b>: %{z}',
                            '<br>%{text}'),
      showlegend=FALSE
    ) %>%
    plotly::layout(
      #    dragmode="pan",
      xaxis = list(
        title="",
        range = c(0,30),
        categoryorder="array",
        categoryarray=paste("Drive", seq.int(1, nr_drives -1), sep = "_"),
        side = "top",
        tickangle = 90,
        rangeslider = list(visible=TRUE)
      ),
      yaxis = list(
        title="",
        fixedrange = TRUE,
        categoryorder="array",
        categoryarray=paste("Stop", seq.int(nr_stops, 1, -1), sep = "_")
      )
      #,annotations =
    )

  return(fig)
}



#' Bt with discrete colorbar
#'
#' @param df input data
#' @param annotated True if you want to add annotations to the plot (Default: False)
#'
#' @return plotly object
#'
#' @importFrom magrittr "%>%"
#'
#' @export
bt <- function(df, annotated=F) {

  aus_kat <- unique(stats::na.omit(df$Ausl_Kat))
  col_palette <- viridis::plasma(length(aus_kat))

  discrete_col <- discrete_coloring(
    categories=aus_kat,
    col_palette=col_palette
  )

  # ======================
  # option 1
  # create poc plot
  # create poc plot
  fig <-
    plotly::plot_ly() %>%
    plotly::add_trace(
      type = "heatmap",
      name = "",
      data = df,
      x = ~fahrt_seq,
      y = ~Haltestellenlangname,
      z = ~Ausl_Kat,
      text = ~FZ_AB,
      hovertemplate = paste('<b>Drive</b>: %{x}',
                            '<br><b>Stop</b>: %{y}',
                            '<br><b>Nr Passengers</b>: %{z}',
                            '<br>%{text}'),
      showlegend=FALSE,
      colorscale=discrete_col$colorscale,
      colorbar=list(
        title="Auslastung",
        len=0.5,
        tickvals=discrete_col$tickvals,
        ticktext=discrete_col$ticktext
      )
    )

  if (annotated){
    fig <- fig %>%
      plotly::add_annotations(
        x = df$fahrt_seq,
        y = df$Haltestellenlangname,
        text = as.character(df$Ausl_Kat),
        showarrow = FALSE,
        ax = 20,
        ay = -20
      )
  }

  fig <-
    fig %>%
    plotly::layout(
      #    dragmode="pan",
      xaxis = list(
        title="",
        tickmode='linear',
        range = c(0,30),
        categoryorder="array",
        categoryarray=unique(df$fahrt_seq[order(as.numeric(df$fahrt_seq))]),
        side = "top",
        tickangle = 90,
        rangeslider = list(visible=TRUE)
      ),
      yaxis = list(
        title="",
        fixedrange = TRUE,
        categoryorder="array",
        categoryarray=unique(df$Haltestellenlangname[order(-df$halt_seq, df$Haltestellenlangname)])
      )
      #,annotations =
    )

  return(plotly::partial_bundle(fig))
}


#' Bt with traces and time
#'
#' @description To be merged with bt_traces
#'
#' @param df input data
#'
#' @return plotly object
#'
#' @importFrom magrittr "%>%"
#'
#' @export
bt_trace_time <- function(df) {

  aus_kat <- unique(stats::na.omit(df$Ausl_Kat))
  col_palette <- viridis::plasma(length(aus_kat))

  df <- df %>%
    dplyr::mutate(
      FZ_AB = lubridate::ymd_hms(paste("2020-06-03", !!rlang::sym('FZ_AB')))
    ) %>%
    dplyr::group_by(
      !!rlang::sym('fahrt_seq')
    ) %>%
    dplyr::mutate(
      abfahrt = min(!!rlang::sym('FZ_AB'))
    ) %>%
    dplyr::ungroup()

  fig <- plotly::plot_ly()

  for (i in seq.int(length.out = length(aus_kat))) {

    a_k <- df %>%
      dplyr::mutate(
        a_k = ifelse(!!rlang::sym('Ausl_Kat') == aus_kat[i], !!rlang::sym('Ausl_Kat'), NA),
        a_k_l =
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
    # a_k <- df %>%
    #   filter(
    #     Ausl_Kat == aus_kat[i]
    #   ) %>%
    #   mutate(
    #     a_k = ifelse(Ausl_Kat == aus_kat[i], Ausl_Kat, NA),
    #     a_k_l =
    #       ifelse(
    #         !is.na(Besetzung),
    #         paste(
    #           '<b>Drive</b>:', fahrt_seq ,
    #           '<br><b>Stop</b>:', Haltestellenlangname,
    #           '<br><b>Nr Passengers</b>:', Besetzung,
    #           '<extra>"A. K."', Ausl_Kat, '</extra>'
    #         ),
    #         paste(
    #           '<b>Drive</b>:', fahrt_seq ,
    #           '<br><b>Stop</b>:', Haltestellenlangname,
    #           '<br><b>Nr Passengers</b>:N/A',
    #           '<extra>"A. K."', Ausl_Kat, '</extra>'
    #         )
    #       )
    #   ) %>%
    #   arrange(Haltestellenlangname, fahrt_seq)

    colorscale <- array(
      data=c(0, 1, rep(col_palette[i], 2)),
      dim= c(2,2)
    )

    if (NROW(a_k) > 0){

      fig <- fig %>%
        plotly::add_trace(
          type = "heatmap",
          name = paste("A. K.", aus_kat[i]),
          data = a_k,
          x = ~abfahrt,
          y = ~Haltestellenlangname,
          z = ~a_k,
          text = ~a_k_l,
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
          legendgroup = paste("A. K.", aus_kat[i])
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
  }

  fig <- fig %>%
    plotly::layout(
      showlegend=T,
      xaxis = list(
        title="",
        # tickmode='linear',
        # range = c(0,30),
        # categoryorder="array",
        # categoryarray=unique(df$fahrt_seq[order(as.numeric(df$fahrt_seq))]),
        side = "top",
        tickangle = 90,
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
        fixedrange = TRUE,
        categoryorder="array",
        categoryarray=unique(df$Haltestellenlangname[order(-df$halt_seq, df$Haltestellenlangname)])
      )
    )

  return(plotly::partial_bundle(fig))

}



#' Bt with traces
#'
#' @description To be merged with bt_traces_time
#'
#' @param df input data
#'
#' @return plotly object
#'
#' @importFrom magrittr "%>%"
#'
#' @export
bt_trace <- function(df) {

  aus_kat <- unique(stats::na.omit(df$Ausl_Kat))
  col_palette <- viridis::plasma(length(aus_kat))

  fig <- plotly::plot_ly()

  for (i in seq.int(length.out = length(aus_kat))) {

    a_k <- df %>%
      dplyr::mutate(
        a_k = ifelse(!!rlang::sym('Ausl_Kat') == aus_kat[i], !!rlang::sym('Ausl_Kat'), NA),
        a_k_l =
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
    # a_k <- df %>%
    #   filter(
    #     Ausl_Kat == aus_kat[i]
    #   ) %>%
    #   mutate(
    #     a_k = ifelse(Ausl_Kat == aus_kat[i], Ausl_Kat, NA),
    #     a_k_l =
    #       ifelse(
    #         !is.na(Besetzung),
    #         paste(
    #           '<b>Drive</b>:', fahrt_seq ,
    #           '<br><b>Stop</b>:', Haltestellenlangname,
    #           '<br><b>Nr Passengers</b>:', Besetzung,
    #           '<extra>"A. K."', Ausl_Kat, '</extra>'
    #         ),
    #         paste(
    #           '<b>Drive</b>:', fahrt_seq ,
    #           '<br><b>Stop</b>:', Haltestellenlangname,
    #           '<br><b>Nr Passengers</b>:N/A',
    #           '<extra>"A. K."', Ausl_Kat, '</extra>'
    #         )
    #       )
    #   ) %>%
    #   arrange(Haltestellenlangname, fahrt_seq)

    colorscale <- array(
      data=c(0, 1, rep(col_palette[i], 2)),
      dim= c(2,2)
    )

    if (NROW(a_k) > 0){

      fig <- fig %>%
        plotly::add_trace(
          type = "heatmap",
          name = paste("A. K.", aus_kat[i]),
          data = a_k,
          x = ~fahrt_seq,
          y = ~Haltestellenlangname,
          z = ~a_k,
          text = ~a_k_l,
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
          legendgroup = paste("A. K.", aus_kat[i])
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
  }

  fig <- fig %>%
    plotly::layout(
      showlegend=T,
      xaxis = list(
        title="",
        tickmode='linear',
        range = c(0,30),
        categoryorder="array",
        categoryarray=unique(df$fahrt_seq[order(as.numeric(df$fahrt_seq))]),
        side = "top",
        tickangle = 90,
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
        fixedrange = TRUE,
        categoryorder="array",
        categoryarray=unique(df$Haltestellenlangname[order(-df$halt_seq, df$Haltestellenlangname)])
      )
    )

  return(plotly::partial_bundle(fig))

}
