library(tidyverse)
library(plotly)

sapply(list.files("./R", full.names = T), source)

data("sample_files")

df <- sample_files[[4]]$data

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

discrete_col <- discrete_coloring(categories=aus_kat, col_palette=col_palette)

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

