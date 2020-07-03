# ---------------------------------------------
# Testing plotting (rudimentary)
context("utils")

categories <- paste("Category", seq.int(5))
col_palette <- viridis::magma(length(categories))

test_that("Test discrete_coloring", {

  dc <- discrete_coloring(categories = categories, col_palette =  col_palette)

  expect_true(length(dc) == 3)

  expect_true(all(names(dc) %in% c("colorscale", "tickvals", "ticktext")))

  expect_true(dim(dc$colorscale)[1] == length(categories) * 2)

  expect_true(dim(dc$colorscale)[2] == 2)

  expect_true(length(dc$tickvals) == length(categories) && length(dc$ticktext) == length(categories))

  expect_true(all(categories == dc$ticktext))

  expect_error(discrete_coloring(categories[1:3], col_palette))

  expect_error(discrete_coloring(categories, col_palette[1:4]))
})
