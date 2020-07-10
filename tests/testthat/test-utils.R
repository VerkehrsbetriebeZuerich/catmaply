# ---------------------------------------------
# Testing plotting
context("utils")

test_that("Test discrete_coloring", {

  categories <- paste("Category", seq.int(5))
  col_palette <- viridis::magma(length(categories))

  dc <- discrete_coloring(categories = categories, col_palette =  col_palette)

  expect_true(is(dc, "list"))

  expect_true(length(dc) == 3)

  expect_true(all(names(dc) %in% c("colorscale", "tickvals", "ticktext")))

  expect_true(dim(dc$colorscale)[1] == length(categories) * 2)

  expect_true(dim(dc$colorscale)[2] == 2)

  expect_true(length(dc$tickvals) == length(categories) && length(dc$ticktext) == length(categories))

  expect_true(all(categories == dc$ticktext))

  expect_error(discrete_coloring(categories[1:3], col_palette))

  expect_error(discrete_coloring(categories, col_palette[1:4]))

})


test_that("Test discrete_coloring with custom color ranges", {

  data("vbz")

  df <- vbz[[3]]$data

  categories <- paste("Category", seq.int(5))
  col_palette <- viridis::magma(length(categories) * 2)

  dc <- discrete_coloring(categories = categories, col_palette =  col_palette, range_max = max(stats::na.omit(df$Besetzung)), range_min = min(stats::na.omit(df$Besetzung)))

  expect_true(is(dc, "list"))

  expect_true(length(dc) == 3)

  expect_true(all(names(dc) %in% c("colorscale", "tickvals", "ticktext")))

  expect_true(dim(dc$colorscale)[1] == length(categories) * 2)

  expect_true(dim(dc$colorscale)[2] == 2)

  expect_true(length(dc$tickvals) == length(categories) && length(dc$ticktext) == length(categories))

  expect_true(all(categories == dc$ticktext))

  expect_error(discrete_coloring(categories[1:3], col_palette))

  expect_error(discrete_coloring(categories, col_palette[1:4]))

})
