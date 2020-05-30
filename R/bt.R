

bt <- function(
  input_data
) {
  df <- df %>% 
    arrange(Drive_id, Stop_id)
  
  # ---------------------------
  # create poc plot
  fig <- 
    plot_ly() %>% 
    add_trace(
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
    layout(
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

