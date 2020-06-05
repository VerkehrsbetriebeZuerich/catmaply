# ---------------------------------------------
# Testing plotting (rudimentary)
context("bt - plotly plots")

data("sample_files")

df <- sample_files[[1]]$data

fig <- bt(df)
expect_true(is(fig, "plotly"))

fig <- bt_trace(df)
expect_true(is(fig, "plotly"))

fig <- bt_trace_time(df)
expect_true(is(fig, "plotly"))

df <- generate_test_data(5, 10)
fig <- bt_demo(df)
expect_true(is(fig, "plotly"))