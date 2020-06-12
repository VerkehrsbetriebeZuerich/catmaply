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
})

test_that("test error_handling", {
  expect_error(catmaply(
    df,
    x='bla',
    x_order = 'fahrt_seq',
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    vals = "Ausl_Kat"
  )
  )

  expect_error(catmaply(
    df,
    x='fahrt_seq',
    x_order = 'fahrt_seq',
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    vals = "Ausl_Kat",
    color_palette = list(1, 2, 3)
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
