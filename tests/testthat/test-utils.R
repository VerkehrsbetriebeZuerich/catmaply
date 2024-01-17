# ---------------------------------------------
# Testing plotting
library(dplyr)
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
  legend_items = c(1,2,3,4)

  dc <- discrete_coloring(df, color_palette, TRUE, legend_items)
  expect_true(all(names(dc) %in% c("colorscale", "tickvals", "ticktext")))
  dc <- discrete_coloring(df, color_palette, FALSE, legend_items)
  expect_true(all(names(dc) %in% c("colorscale", "tickvals", "ticktext")))

})



test_that("Test discrete_coloring_range", {

  df <- tibble(
    x=as.integer(c(1,1,1,1,2,2,2,2)),
    x_order=as.integer(c(1,1,1,1,2,2,2,2)),
    y=as.integer(c(1,2,3,4,1,2,3,4)),
    z=as.integer(c(1,3,5,7,11,6,4,2)),
    category=as.integer(c(1,2,3,4,4,3,2,1)),
    legend=as.character(as.integer(c(1,2,3,4,4,3,2,1)))
  )

  color_palette = viridis::inferno(4)

  dc <- discrete_coloring_range(df, color_palette, TRUE)

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

  expect_error(discrete_coloring_range(select(df, x, y), color_palette, TRUE))

  expect_error(discrete_coloring_range(df, viridis::inferno(2), TRUE))

  # factor
  df <- tibble(
    x=as.integer(c(1,1,1,1,2,2,2,2)),
    x_order=as.integer(c(1,1,1,1,2,2,2,2)),
    y=as.integer(c(1,2,3,4,1,2,3,4)),
    z=as.integer(c(1,3,5,7,11,6,4,2)),
    category=as.factor(as.integer(c(1,2,3,4,4,3,2,1))),
    legend=as.character(as.integer(c(1,2,3,4,4,3,2,1)))
  )

  dc <- discrete_coloring_range(df, viridis::inferno(8), TRUE)
  expect_true(is(dc, "list"))
})


test_that("Test discrete_coloring_range with custom color ranges", {

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

  dc <- discrete_coloring_range(df, color_palette, TRUE)

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

  expect_error(discrete_coloring_range(categories[1:3], col_palette, TRUE))

  expect_error(discrete_coloring_range(categories, col_palette[1:4], TRUE))

})

test_that("Test discrete_coloring_range input error handling", {

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
    discrete_coloring_range(
      df = c("one", "two", "Three"),
      color_palette = color_palette,
      categorical_color_range = FALSE
    )
  )
  expect_error(
    discrete_coloring_range(
      df = df,
      color_palette = as.list(color_palette),
      categorical_color_range = FALSE
    )
  )

})


test_that("Test overlapping categories error discrete_coloring_range", {

  df <- tibble(
    x=as.integer(c(1,1,1,1,2,2,2,2)),
    y=as.integer(c(1,2,3,4,1,2,3,4)),
    z=as.integer(c(1,3,5,11,1.5,3.5,5.5,1.15)),
    category=as.integer(c(1,3,5,11,1,3,5,11)),
    legend=as.character(c(1,3,5,11,1,3,5,11))
  )

  color_palette = viridis::inferno(8)

  expect_error(
    discrete_coloring_range(
      df = df,
      color_palette = color_palette,
      categorical_color_range = FALSE
    )
  )

})

test_that("Test narrow legend lines warning discrete_coloring_range", {

  df <- tibble(
    x=as.integer(c(1,1,1,1,2,2,2,2)),
    y=as.integer(c(1,2,3,4,1,2,3,4)),
    z=as.integer(c(1,3,5,11,1.5,3.5,5.5,11)),
    category=as.integer(c(1,3,5,11,1,3,5,11)),
    legend=as.character(c(1,3,5,11,1,3,5,11))
  )

  color_palette = viridis::inferno(8)

  expect_warning(
    discrete_coloring_range(
      df = df,
      color_palette = color_palette,
      categorical_color_range = TRUE
    )
  )

})


test_that("Test discrete_coloring_categorical", {

  categories <- paste("Category", seq.int(5))
  col_palette <- viridis::magma(length(categories))

  dc <- discrete_coloring_categorical(categories = categories, col_palette =  col_palette)

  expect_true(is(dc, "list"))

  expect_true(length(dc) == 3)

  expect_true(all(names(dc) %in% c("colorscale", "tickvals", "ticktext")))

  expect_true(dim(dc$colorscale)[1] == length(categories) * 2)

  expect_true(dim(dc$colorscale)[2] == 2)

  expect_true(length(dc$tickvals) == length(categories) && length(dc$ticktext) == length(categories))

  expect_true(all(categories == dc$ticktext))

  expect_error(discrete_coloring_categorical(categories[1:3], col_palette))

  expect_error(discrete_coloring_categorical(categories, col_palette[1:4]))

  # factor
  categories <- as.factor(paste("Category", seq.int(5)))
  col_palette <- viridis::magma(length(categories))

  dc <- discrete_coloring_categorical(categories = categories, col_palette =  col_palette)
  expect_true(is(dc, "list"))
})


test_that("Test discrete_coloring_categorical with custom color ranges", {

  df <- vbz[[3]]

  categories <- paste("Category", seq.int(5))
  col_palette <- viridis::magma(length(categories) * 2)

  dc <- discrete_coloring_categorical(categories = categories, col_palette =  col_palette, range_max = max(stats::na.omit(df$occupancy)), range_min = min(stats::na.omit(df$occupancy)))

  expect_true(is(dc, "list"))

  expect_true(length(dc) == 3)

  expect_true(all(names(dc) %in% c("colorscale", "tickvals", "ticktext")))

  expect_true(dim(dc$colorscale)[1] == length(categories) * 2)

  expect_true(dim(dc$colorscale)[2] == 2)

  expect_true(length(dc$tickvals) == length(categories) && length(dc$ticktext) == length(categories))

  expect_true(all(categories == dc$ticktext))

  expect_error(discrete_coloring_categorical(categories[1:3], col_palette))

  expect_error(discrete_coloring_categorical(categories, col_palette[1:4]))

})

test_that("Test discrete_coloring_categorical input error handling", {

  df <- vbz[[3]]

  categories <- paste("Category", seq.int(5))
  col_palette <- viridis::magma(length(categories) * 2)

  expect_error(
    discrete_coloring_categorical(
      categories = as.list(categories),
      col_palette =  col_palette,
      range_max = max(stats::na.omit(df$Besetzung)),
      range_min = min(stats::na.omit(df$Besetzung))
    )
  )
  expect_error(
    discrete_coloring_categorical(
      categories = categories,
      col_palette =  as.list(col_palette),
      range_max = max(stats::na.omit(df$Besetzung)),
      range_min = min(stats::na.omit(df$Besetzung))
    )
  )


})
