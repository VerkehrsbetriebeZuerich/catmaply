# ---------------------------------------------
# Testing plotting
context("utils")

test_that("Test discrete_coloring", {

  df <- tibble(
    x=as.integer(c(1,1,1,1,2,2,2,2)),
    x_order=as.integer(c(1,1,1,1,2,2,2,2)),
    y=as.integer(c(1,2,3,4,1,2,3,4)),
    z=as.integer(c(1,3,5,7,11,6,4,2)),
    category=as.integer(c(1,2,3,4,4,3,2,1)),
    legend=as.character(as.integer(c(1,2,3,4,4,3,2,1)))
  )

  color_palette = viridis::inferno(4)

  dc <- discrete_coloring(df, color_palette)

  expect_true(is(dc, "list"))

  expect_true(length(dc) == 3)

  expect_true(all(names(dc) %in% c("colorscale", "tickvals", "ticktext")))

  expect_true(
    dim(dc$colorscale)[1] ==
      (length(unique(df$category)) * 2) +
      ((length(unique(df$category)) - 1) * 2))

  expect_true(dim(dc$colorscale)[2] == 2)

  expect_true(length(dc$tickvals) == 4 && length(dc$ticktext) == 4)

  expect_true(all(c(1,2,3,4) == dc$ticktext))

  expect_error(discrete_coloring(select(df, x, y), color_palette))

  expect_error(discrete_coloring(df, viridis::inferno(2)))

  # factor
  df <- tibble(
    x=as.integer(c(1,1,1,1,2,2,2,2)),
    x_order=as.integer(c(1,1,1,1,2,2,2,2)),
    y=as.integer(c(1,2,3,4,1,2,3,4)),
    z=as.integer(c(1,3,5,7,11,6,4,2)),
    category=as.factor(as.integer(c(1,2,3,4,4,3,2,1))),
    legend=as.character(as.integer(c(1,2,3,4,4,3,2,1)))
  )

  dc <- discrete_coloring(df, viridis::inferno(8))
  expect_true(is(dc, "list"))
})


test_that("Test discrete_coloring with custom color ranges", {

  df <- tibble(
    x=as.integer(c(1,1,1,1,2,2,2,2)),
    x_order=as.integer(c(1,1,1,1,2,2,2,2)),
    y=as.integer(c(1,2,3,4,1,2,3,4)),
    z=as.integer(c(1,3,5,7,11,6,4,2)),
    category=as.integer(c(1,2,3,4,4,3,2,1)),
    legend=as.character(as.integer(c(1,2,3,4,4,3,2,1)))
  )

  color_palette = viridis::inferno(8)
  categories = unique(df$category)
  len_category = length(unique(df$category))

  dc <- discrete_coloring(df, color_palette)

  expect_true(is(dc, "list"))

  expect_true(length(dc) == 3)

  expect_true(all(names(dc) %in% c("colorscale", "tickvals", "ticktext")))

  expect_true(
    dim(dc$colorscale)[1] ==
      (len_category * 2) + ((len_category - 1) * 2)
  )

  expect_true(dim(dc$colorscale)[2] == 2)

  expect_true(length(dc$tickvals) == len_category && length(dc$ticktext) == len_category)

  expect_true(all(categories == dc$ticktext))

  expect_error(discrete_coloring(categories[1:3], col_palette))

  expect_error(discrete_coloring(categories, col_palette[1:4]))

})

test_that("Test discrete_coloring input error handling", {

  df <- tibble(
    x=as.integer(c(1,1,1,1,2,2,2,2)),
    x_order=as.integer(c(1,1,1,1,2,2,2,2)),
    y=as.integer(c(1,2,3,4,1,2,3,4)),
    z=as.integer(c(1,3,5,7,11,6,4,2)),
    category=as.integer(c(1,2,3,4,4,3,2,1)),
    legend=as.character(as.integer(c(1,2,3,4,4,3,2,1)))
  )

  color_palette = viridis::inferno(8)

  expect_error(
    discrete_coloring(
      df = c("one", "two", "Three"),
      color_palette = color_palette,
    )
  )
  expect_error(
    discrete_coloring(
      df = df,
      color_palette = as.list(color_palette)
    )
  )


})
