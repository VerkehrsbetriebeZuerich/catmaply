# ---------------------------------------------
# Testing plotting (rudimentary)
context("catmaply - plotly plots")

df <- vbz[[2]]$data

test_that("test catmaply", {
  fig <- catmaply_single(df)
  expect_true(is(fig, "plotly"))
})

test_that("test catmaply_trace", {
  fig <- catmaply(
    df,
    x='fahrt_seq',
    x_order = 'fahrt_seq',
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    vals = "Ausl_Kat"
  )
  expect_true(is(fig, "plotly"))

  fig <- catmaply(
    df,
    x=fahrt_seq,
    x_order = fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    vals = Ausl_Kat
  )
  expect_true(is(fig, "plotly"))


  fig <- catmaply(
    df,
    x=fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    vals = Ausl_Kat,
    legend_col = Ausl_Kat
  )
  expect_true(is(fig, "plotly"))

  fig <- catmaply(
    df,
    x=fahrt_seq,
    y = Haltestellenlangname,
    y_order = halt_seq,
    vals = Ausl_Kat
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
      vals = "Ausl_Kat"
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
      vals = "Ausl_Kat",
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
      vals = "Ausl_Kat",
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
      vals = "Ausl_Kat"
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
      vals = "Ausl_Kat"
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
      vals = "Ausl_Kat"
    )
  )

  # check tick legend col
  expect_error(
    catmaply(
      df,
      x='fahrt_seq',
      y = "Haltestellenlangname",
      y_order = "halt_seq",
      vals = "Ausl_Kat",
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
      vals = "Ausl_Kat",
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
      vals = "Ausl_Kat",
      legend_col = c(1,2,3,4)
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
      vals = "Besetzung",
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
        vals = "Besetzung",
        categorical_colorbar = T,
        categorical_col = 'Ausl_Kat',
        color_palette = viridis::inferno
      ),
      "plotly"
    )
  )
})

test_that("test catmaply_trace_time", {
  fig <- catmaply_trace_time(df)
  expect_true(is(fig, "plotly"))
})

test_that("test catmaply_demo", {
  df <- generate_test_data(5, 10)
  fig <- catmaply_demo(df)
  expect_true(is(fig, "plotly"))
})
