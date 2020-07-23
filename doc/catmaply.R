## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE---------------------------------------------------------
library(tidyverse)
library(plotly)
library(lubridate)

## -----------------------------------------------------------------------------
# make sure that you have the corrent RTools installed.
# as you might need to build some packages from source
# if you don't have RTools installed, you can install it with:
# install.packages('installr'); install.Rtools() # not tested on windows
# or download it from here:
# https://cran.r-project.org/bin/windows/Rtools/
# in any case, make sure that you select the correct version, 
# otherwise the installation will fail.
# then you'll need devtools
# if (!require('devtools'))
  # install.packages('devtools')
# finally install the package
# devtools::install_github('yvesmauron/catmaply')

## -----------------------------------------------------------------------------
#install.packages("catmaply")

## -----------------------------------------------------------------------------
library(catmaply)

## -----------------------------------------------------------------------------
data("vbz")
df <- vbz[[3]]$data

knitr::kable(head(df, 10))

## ----out.width = '100%', fig.height=5, warning = FALSE------------------------
catmaply(
    df,
    x='fahrt_seq',
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    z = "Ausl_Kat"
  )

## ----out.width = '100%', fig.height=5, warning = FALSE------------------------
catmaply(
    df,
    x='fahrt_seq',
    x_order = 'fahrt_seq',
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    z = "Besetzung",
    categorical_colorbar = T,
    categorical_col = 'Ausl_Kat',
    color_palette = viridis::magma
  )

## ----out.width = '100%', fig.height=5, warning = FALSE------------------------
catmaply(
    df,
    x='fahrt_seq',
    x_order = 'fahrt_seq',
    x_tickangle = 15,
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    z = "Besetzung",
    categorical_colorbar = T,
    categorical_col = 'Ausl_Kat',
    color_palette = viridis::magma,
    font_color = '#6D65AB',
    font_size = 10
  )

## ----out.width = '100%', fig.height=5, warning = FALSE------------------------
catmaply(
    df,
    x='fahrt_seq',
    x_order = 'fahrt_seq',
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    z = "Besetzung",
    categorical_colorbar = T,
    categorical_col = 'Ausl_Kat',
    color_palette = viridis::magma
  )

## ----out.width = '100%', fig.height=5, warning = FALSE------------------------
catmaply(
    df,
    x='fahrt_seq',
    x_order = 'fahrt_seq',
    x_tickangle = 15,
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    z = "Besetzung",
    categorical_colorbar = T,
    categorical_col = 'Ausl_Kat',
    color_palette = viridis::magma,
    font_color = '#6D65AB',
    font_size = 10
  )

## ----out.width = '100%', fig.height=5, warning = FALSE------------------------
catmaply(
  df,
  x=fahrt_seq,
  x_order = fahrt_seq,
  x_tickangle = 15,
  y = Haltestellenlangname,
  y_order = halt_seq,
  z = Besetzung,
  categorical_colorbar = T,
  categorical_col = Ausl_Kat,
  color_palette = viridis::inferno,
  hover_template = paste(
    '<b>Fahrt Nr.</b>:', fahrt_seq,
    '<br><b>Haltestelle</b>:', Haltestellenlangname,
    '<br><b>Auslastung</b>:', Ausl_Kat,
    '<br><b>Besetzung</b>:', round(Besetzung, 2),
    '<extra></extra>'
  )
)

## ----out.width = '100%', fig.height=5, warning = FALSE------------------------
df <- df %>% 
  mutate(
    legend_col = paste("Kategorie", Ausl_Kat)
  )

catmaply(
  df,
  x=fahrt_seq,
  x_order = fahrt_seq,
  x_tickangle = 15,
  y = Haltestellenlangname,
  y_order = halt_seq,
  z = Besetzung,
  categorical_colorbar = T,
  categorical_col = Ausl_Kat,
  color_palette = viridis::inferno,
  hover_template = paste(
    '<b>Fahrt Nr.</b>:', fahrt_seq,
    '<br><b>Haltestelle</b>:', Haltestellenlangname,
    '<br><b>Auslastung</b>:', Ausl_Kat,
    '<br><b>Besetzung</b>:', round(Besetzung, 2),
    '<extra></extra>'
  ),
  legend_col = legend_col
)

## ----out.width = '100%', fig.height=5, warning = FALSE------------------------
catmaply(
  df,
  x=fahrt_seq,
  x_order = fahrt_seq,
  x_tickangle = 15,
  y = Haltestellenlangname,
  y_order = halt_seq,
  z = Ausl_Kat,
  color_palette = viridis::inferno,
  hover_template = paste(
    '<b>Fahrt Nr.</b>:', fahrt_seq,
    '<br><b>Haltestelle</b>:', Haltestellenlangname,
    '<br><b>Auslastung</b>:', Ausl_Kat,
    '<br><b>Besetzung</b>:', round(Besetzung, 2),
    '<extra></extra>'
  ),
  legend_interactive = F
)

## ----out.width = '100%', fig.height=5, warning = FALSE------------------------
catmaply(
  df,
  x=fahrt_seq,
  x_order = fahrt_seq,
  x_tickangle = 15,
  y = Haltestellenlangname,
  y_order = halt_seq,
  z = Ausl_Kat,
  color_palette = viridis::inferno,
  hover_template = paste(
    '<b>Fahrt Nr.</b>:', fahrt_seq,
    '<br><b>Haltestelle</b>:', Haltestellenlangname,
    '<br><b>Auslastung</b>:', Ausl_Kat,
    '<br><b>Besetzung</b>:', round(Besetzung, 2),
    '<extra></extra>'
  ),
  legend = F
)

## ----out.width = '100%', fig.height=5, warning = FALSE------------------------
df <- df %>%
  dplyr::mutate(
    FZ_AB = lubridate::ymd_hms(paste("2020-06-03", !!rlang::sym('FZ_AB')))
  ) %>%
  dplyr::group_by(
    !!rlang::sym('fahrt_seq')
  ) %>%
  dplyr::mutate(
    departure = min(!!rlang::sym('FZ_AB'))
  ) %>%
  dplyr::ungroup()

catmaply(
  df,
  x=departure,
  y = Haltestellenlangname,
  y_order = halt_seq,
  z = Besetzung,
  categorical_colorbar = T,
  categorical_col = Ausl_Kat,
  color_palette = viridis::inferno,
  hover_template = paste(
    '<b>Fahrt Nr.</b>:', fahrt_seq,
    '<br><b>Haltestelle</b>:', Haltestellenlangname,
    '<br><b>Auslastung</b>:', Ausl_Kat,
    '<br><b>Besetzung</b>:', round(Besetzung, 2),
    '<extra></extra>'
  )
)

## ----out.width = '100%', fig.height=5, warning = FALSE------------------------

catmaply(
  df,
  x=departure,
  y = Haltestellenlangname,
  y_order = halt_seq,
  z = Besetzung,
  categorical_colorbar = T,
  categorical_col = Ausl_Kat,
  color_palette = viridis::inferno,
  hover_template = paste(
    '<b>Fahrt Nr.</b>:', fahrt_seq,
    '<br><b>Haltestelle</b>:', Haltestellenlangname,
    '<br><b>Auslastung</b>:', Ausl_Kat,
    '<br><b>Besetzung</b>:', round(Besetzung, 2),
    '<extra></extra>'
  ),
  tickformatstops=list(
    list(dtickrange = list(NULL, 1000), value = "%H:%M:%S.%L"),
    list(dtickrange = list(1000, 60000), value = "%H:%M:%S"),
    list(dtickrange = list(60000, 3600000), value = "%H:%M"),
    list(dtickrange = list(3600000, 86400000), value = "%H:%M"),
    list(dtickrange = list(86400000, 604800000), value = "%H:%M"),
    list(dtickrange = list(604800000, "M1"), value = "%H:%M"),
    list(dtickrange = list("M1", "M12"), value = "%H:%M"),
    list(dtickrange = list("M12", NULL), value = "%H:%M")
  )
)

