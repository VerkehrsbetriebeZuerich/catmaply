library(tidyverse)
library(plotly)

sapply(list.files("./R", full.names = T), source)

data("sample_files")

df <- sample_files[[2]]$data

df <- df %>% #filter(fahrt_seq == 116) %>%
  mutate(
    cat = factor(Ausl_Kat, levels=order(unique(df$Ausl_Kat)), labels = paste("Auslastungs Kategorie", order(unique(df$Ausl_Kat))))
  )


#' Generates the parameters necessary for dicrete coloring and colorbar
#'
#' @param categories categories, for coloring should done
#' @param col_palette the color palette
#'
#' @return list(colorscale, tickvals, ticktext)
#' @export
discrete_coloring <- function(categories, col_palette) {
  # TODO: create ticktext dynamically
  bvals <- c(0, order(categories))
  pal <- col_palette

  if (length(bvals) != (length(pal) + 1)) {
    stop("length(bvals) should be equal to (length(pal) -1)")
  }

  bvals <- bvals[order(bvals)]
  nvals <- (bvals - min(bvals)) / (max(bvals) - min(bvals))

  dcolorscale <- array(NA, dim = c((length(nvals) * 2) -2, 2))

  for (i in seq.int(length.out = length(nvals) -1 )) {
    index <- ((i - 1) * 2) + 1
    dcolorscale[index,] <- c(nvals[i], pal[i])
    dcolorscale[index + 1,] <- c(nvals[i + 1], pal[i])
  }

  # calculate tick values for legend (lowest point to max point)
  # works only with even spacing until now
  ticks <- seq.int(from = 1, to = max(bvals) * 2, by = 1)
  # calc percentage of ticks * range (max - min) + min
  tick_vals <- (ticks[ticks %% 2 != 0] / max(ticks)) * (max(bvals) - min(tail(bvals, -1))) + min(tail(bvals, -1))
  tick_text <- paste("Kat.", tail(bvals, -1))

  return(
    list(
      colorscale=dcolorscale,
      tickvals=tick_vals,
      ticktext=tick_text
    )
  )
}

aus_kat <- unique(na.omit(df$Ausl_Kat))
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
  plot_ly() %>%
  add_trace(
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
  ) %>%
  layout(
    #    dragmode="pan",
    xaxis = list(
      title="",
      tickmode='linear',
      range = c(0,30),
      categoryorder="array",
      categoryarray=unique(df$fahrt_seq[order(df$fahrt_seq)]),
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

fig



# ======================
# option 2
i <- 1


fig <- plot_ly()
a_k <- df %>%
  filter(
    Ausl_Kat == aus_kat[i]
  )

a_k <- a_k %>%
  mutate(
    rand = round(rnorm(NROW(a_k)), 2),
    Haltestellenlangname = as.character(a_k$Haltestellenlangname)
  ) %>%
  as_tibble()

colorscale <- array(
  c(0, 1, rep(col_palette[i], 2)), dim= c(2,2)
)

fig <- fig %>%
  add_trace(
    type = "heatmap",
    name = paste("A. K.", aus_kat[i]),
    data = a_k,
    x = ~fahrt_seq,
    y = ~Haltestellenlangname,
    z = ~rand
    # text = ~Haltestellenlangname,
    # hovertemplate = '%{text}'
    # # paste(
    #   '<b>Drive</b>: %{x}',
    #   '<br><b>Stop</b>: %{y}',
    #   '<br><b>Nr Passengers</b>: %{z}',
    #   '<br>%{text}'
    # ),
    # colorscale=colorscale,
    # showlegend=T,
    # showscale=F,
    # legendgroup = paste("A. K.", aus_kat[i])
  ) %>%
  add_annotations(x = a_k$fahrt_seq,
                  y = a_k$Haltestellenlangname,
                  text = a_k$rand,
                  showarrow = FALSE,
                  ax = 20,
                  ay = -20)

fig
#
# fig <- fig %>%
#   layout(
#     showlegend=T,
#     xaxis = list(
#       rangeslider = list(visible=TRUE)
#     )
#   )
#
# fig

fig <- plot_ly()
for (i in seq.int(length.out = length(aus_kat))) {

  a_k <- df %>%
    mutate(
      a_k = ifelse(Ausl_Kat == aus_kat[i], Ausl_Kat, NA),
      a_k_l =
        ifelse(
          !is.na(Besetzung),
          paste(
            '<b>Drive</b>:', fahrt_seq ,
            '<br><b>Stop</b>:', Haltestellenlangname,
            '<br><b>Nr Passengers</b>:', Besetzung,
            '<extra>"A. K."', Ausl_Kat, '</extra>'
          ),
          paste(
            '<b>Drive</b>:', fahrt_seq ,
            '<br><b>Stop</b>:', Haltestellenlangname,
            '<br><b>Nr Passengers</b>:N/A',
            '<extra>"A. K."', Ausl_Kat, '</extra>'
          )
        )
    )
  # a_k <- df %>%
  #   filter(
  #     Ausl_Kat == aus_kat[i]
  #   )

  colorscale <- array(
    c(0, 1, rep(col_palette[i], 2)), dim= c(2,2)
  )

  if (NROW(a_k) > 0){
    fig <- fig %>%
      add_trace(
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
      ) %>%
      add_annotations(
        x = a_k$fahrt_seq,
        y = a_k$Haltestellenlangname,
        text = a_k$a_k,
        showarrow = FALSE,
        ax = 20,
        ay = -20
      )
  }
}



fig <- fig %>%
  layout(
    showlegend=T,
    xaxis = list(
      title="",
      tickmode='linear',
      range = c(0,30),
      categoryorder="array",
      categoryarray=unique(df$fahrt_seq[order(df$fahrt_seq)]),
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
  )

fig



test_data <- generate_test_data()

a_df <- test_data %>%
  filter(
    Drive_id > 10 & Stop_id > 20
  )

b_df <- test_data %>%
  filter(
    Drive_id <= 25 & Stop_id <= 20
  )


fig <-
  plot_ly() %>%
  add_trace(
    type = "heatmap",
    name = "a",
    data = a_df,
    x = ~Drive_names,
    y = ~Stop_names,
    z = ~Passengers,
    text = ~Some_label,
    hovertemplate = paste('<b>Drive</b>: %{x}',
                          '<br><b>Stop</b>: %{y}',
                          '<br><b>Nr Passengers</b>: %{z}',
                          '<br>%{text}'),
    showlegend=T,
    showscale=F,
    legendgroup = "a"
  ) %>%
  add_trace(
    type = "heatmap",
    name = "b",
    data = b_df,
    x = ~Drive_names,
    y = ~Stop_names,
    z = ~Passengers,
    text = ~Some_label,
    hovertemplate = paste('<b>Drive</b>: %{x}',
                          '<br><b>Stop</b>: %{y}',
                          '<br><b>Nr Passengers</b>: %{z}',
                          '<br>%{text}'),
    showlegend=T,
    showscale=F,
    legendgroup = "b"
  ) %>%
  # add_trace(
  #   type="scatter",
  #   mode="markers",
  #   x=c(NA),
  #   y=c(NA),
  #   marker=list(
  #     size = 10,
  #     color= "green"
  #   ),
  #   legendgroup="a",
  #   showlegend=T,
  #   name="a"
  # ) %>%
  # add_trace(
  #   type="scatter",
  #   mode="markers",
  #   x=c(NA),
  #   y=c(NA),
  #   marker=list(
  #     size = 10,
  #     color= "red"
  #   ),
  #   legendgroup="b",
  #   showlegend=T,
  #   name="b"
  # ) %>%
  layout(
    showlegend=T,
    xaxis = list(
      rangeslider = list(visible=TRUE)
    )
  )

fig
