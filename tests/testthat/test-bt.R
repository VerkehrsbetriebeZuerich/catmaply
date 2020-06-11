# ---------------------------------------------
# Testing plotting (rudimentary)
context("bt - plotly plots")

data("sample_files")

df <- sample_files[[2]]$data

test_that("test bt", {
  fig <- bt(df)
  expect_true(is(fig, "plotly"))
})

test_that("test bt_trace", {
  fig <- bt_trace(
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
  expect_error(bt_trace(
    df,
    x='bla',
    x_order = 'fahrt_seq',
    y = "Haltestellenlangname",
    y_order = "halt_seq",
    vals = "Ausl_Kat"
  )
  )

  expect_error(bt_trace(
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

test_that("test bt_trace_time", {
  fig <- bt_trace_time(df)
  expect_true(is(fig, "plotly"))
})

test_that("test bt_demo", {
  df <- generate_test_data(5, 10)
  fig <- bt_demo(df)
  expect_true(is(fig, "plotly"))
})
