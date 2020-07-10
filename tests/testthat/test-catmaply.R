# ---------------------------------------------
# Testing plotting (rudimentary)
context("catmaply")

df <- vbz[[3]]$data

test_that("test catmaply", {

  fig <- catmaply(
    df,
    x='fahrt_seq',
    x_order = 'fahrt_seq',
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    z = "Ausl_Kat"
  )
  expect_true(is(fig, "plotly"))


  fig <- catmaply(
    df,
    x=fahrt_seq,
    x_order = fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    z = Ausl_Kat
  )
  expect_true(is(fig, "plotly"))


  fig <- catmaply(
    df,
    x=fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    z = Ausl_Kat,
    legend_col = Ausl_Kat
  )
  expect_true(is(fig, "plotly"))

  fig <- catmaply(
    df,
    x=fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    z = Ausl_Kat
  )
  expect_true(is(fig, "plotly"))

  fig <- catmaply(
    df,
    x = fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    z = Ausl_Kat,
    hover_hide = F,
    hover_template = paste(
      '<b>This</b>:', fahrt_seq,
      '<br><b>That</b>:', halt_seq,
      '<br><b>here</b>:', Ausl_Kat,
      '<extra></extra>'
    )
  )
  expect_true(is(fig, "plotly"))

  fig <- catmaply(
    df,
    x = fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    z = Ausl_Kat,
    hover_hide = T,
    hover_template = paste(
      '<b>This</b>:', fahrt_seq,
      '<br><b>That</b>:', halt_seq,
      '<br><b>here</b>:', Ausl_Kat,
      '<extra></extra>'
    )
  )
  expect_true(is(fig, "plotly"))


  fig <- catmaply(
    df,
    x = fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    z = Ausl_Kat,
    hover_hide = F
  )
  expect_true(is(fig, "plotly"))



  fig <- catmaply(
    df,
    x = fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    z = Ausl_Kat,
    hover_template = paste(Haltestellenlangname)
  )
  expect_true(is(fig, "plotly"))


  fig <- catmaply(
    df,
    x = fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    z = Ausl_Kat,
    hover_template = paste(Haltestellenlangname),
    legend_interactive = F
  )
  expect_true(is(fig, "plotly"))

  fig <- catmaply(
    df,
    x = fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    z = Ausl_Kat,
    hover_template = paste(Haltestellenlangname),
    legend_interactive = T,
    legend = F
  )
  expect_true(is(fig, "plotly"))

  fig <- catmaply(
    df,
    x = fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    z = Ausl_Kat,
    hover_template = paste(Haltestellenlangname),
    legend_interactive = F,
    legend = F
  )
  expect_true(is(fig, "plotly"))

  fig <- catmaply(
    df,
    x = fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    z = Ausl_Kat,
    hover_template = paste(Haltestellenlangname),
    rangeslider = T
  )
  expect_true(is(fig, "plotly"))

  fig <- catmaply(
    df,
    x = fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    z = Ausl_Kat,
    hover_template = paste(Haltestellenlangname),
    rangeslider = F
  )
  expect_true(is(fig, "plotly"))

  fig <- catmaply(
    df,
    x='fahrt_seq',
    x_order = 'fahrt_seq',
    x_init_range = 2,
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    z = "Ausl_Kat"
  )
  expect_true(is(fig, "plotly"))
})


test_that("test time axis", {
  library(dplyr)

  df <- df %>%
    dplyr::mutate(
      fahrt_name = paste("Abfahrt", fahrt_seq),
      FZ_AB = lubridate::ymd_hms(paste("2020-06-03", !!rlang::sym('FZ_AB')))
    ) %>%
    dplyr::group_by(
      !!rlang::sym('fahrt_seq')
    ) %>%
    dplyr::mutate(
      abfahrt = min(!!rlang::sym('FZ_AB'))
    ) %>%
    dplyr::ungroup()

  fig <- catmaply(
    df,
    x=abfahrt,
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    z = "Ausl_Kat",
    legend_interactive = T
  )

  expect_true(is(fig, "plotly"))


  fig <- catmaply(
    df,
    x=abfahrt,
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    z = "Ausl_Kat",
    legend_interactive = F
  )

  expect_true(is(fig, "plotly"))


  fig <- catmaply(
    df,
    x=abfahrt,
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    z = "Ausl_Kat",
    legend_interactive = T,
    legend = F
  )

  expect_true(is(fig, "plotly"))


  fig <- catmaply(
    df,
    x=abfahrt,
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    z = "Ausl_Kat",
    legend_interactive = F,
    legend = F
  )

  expect_true(is(fig, "plotly"))

})



test_that("test error_handling", {
  # non-existant column name
  expect_error(
    catmaply(
      df,
      x='bla',
      x_order = 'fahrt_seq',
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat"
    )
  )
  # wrong color palette data type
  expect_error(
    catmaply(
      df,
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat",
      color_palette = list(1, 2, 3)
    )
  )

  # too little values
  # wrong color palette
  expect_error(
    catmaply(
      df,
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat",
      color_palette = c("#444", "#444", "#444")
    )
  )

  # wrong side for x axis
  expect_error(
    catmaply(
      df,
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      x_side = 'left',
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat"
    )
  )

  # wrong side for y axis
  expect_error(
    catmaply(
      df,
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      x_side = 'the other one',
      z = "Ausl_Kat"
    )
  )

  # check tick angle range
  expect_error(
    catmaply(
      df,
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      x_tickangle = -9000,
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat"
    )
  )

  # check tick legend col
  expect_error(
    catmaply(
      df,
      x='fahrt_seq',
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat",
      legend_col = halt_seq
    )
  )

  # check tick legend col
  expect_error(
    catmaply(
      df,
      x='fahrt_seq',
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat",
      legend_col = "bla"
    )
  )

  # check tick legend col
  expect_error(
    catmaply(
      df,
      x='fahrt_seq',
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat",
      legend_col = c(1,2,3,4)
    )
  )

  # template references wrong column
  expect_error(
    catmaply(
      df,
      x = fahrt_seq,
      y = Haltestellenlangname,
      y_order = halt_seq,
      z = Ausl_Kat,
      hover_template = paste(fahrt_seq, fs)
    )
  )

  expect_error(
    catmaply(
      as.matrix(df),
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat"
    )
  )

  expect_error(
    catmaply(
      as.matrix(df),
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat",
      rangeslider = "bla"
    )
  )

  expect_error(
    catmaply(
      as.matrix(df),
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat"
    )
  )

  expect_error(
    catmaply(
      df,
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      x_init_range = "0",
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat",
    )
  )

  expect_warning(
    catmaply(
      df,
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      x_init_range = 1,
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Ausl_Kat",
    )
  )
})

# check colorbar
test_that("test catmaply colorbar", {

  expect_error(
    catmaply(
      df,
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      x_tickangle = -10,
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Besetzung",
      categorical_colorbar = T,
      categorical_col = 'Ausl_Kat',
      color_palette = viridis::inferno(6)
    )
  )

  expect_true(
    is(
      catmaply(
        df,
        x='fahrt_seq',
        x_order = 'fahrt_seq',
        x_tickangle = -10,
        y = "Haltestellenlangname",
        y_order = "halt_seq",
        z = "Besetzung",
        categorical_colorbar = T,
        categorical_col = 'Ausl_Kat',
        color_palette = viridis::inferno
      ),
      "plotly"
    )
  )

  expect_true(
    is(
      catmaply(
        df,
        x='fahrt_seq',
        x_order = 'fahrt_seq',
        x_tickangle = -10,
        y = "Haltestellenlangname",
        y_order = "halt_seq",
        z = "Besetzung",
        categorical_colorbar = T,
        categorical_col = 'Ausl_Kat',
        color_palette = viridis::inferno,
        legend_interactive = F,
        legend = T
      ),
      "plotly"
    )
  )

  expect_warning(
    catmaply(
      df,
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      x_tickangle = -10,
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Besetzung",
      categorical_colorbar = T,
      categorical_col = 'Ausl_Kat',
      color_palette = viridis::inferno,
      legend = F
    )
  )

  expect_error(
    catmaply(
      df,
      x='fahrt_seq',
      x_order = 'fahrt_seq',
      x_tickangle = -10,
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      z = "Besetzung",
      categorical_colorbar = T,
      categorical_col = 'Ausl_Kat',
      color_palette = viridis::inferno(6),
      legend = T
    )
  )
})
