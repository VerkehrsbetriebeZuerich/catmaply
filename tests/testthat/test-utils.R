# ---------------------------------------------
# Testing coloring/palettes
context("utils")

test_that("discrete_coloring", {
  # discrete color palette
  cats <- seq.int(5)
  col_pal <- viridis::inferno(length(cats))

  dc <- discrete_coloring(cats, col_pal)
  # for each category a tick
  expect_equal(length(dc$tickvals), length(cats))
  expect_equal(length(dc$ticktext), length(cats))
  # colorscale should have upper and lower limits per category (len(cats) * 2)
  expect_equal(NROW(dc$colorscale), length(cats) * 2)
  # but only one color per cat should be in colorscale
  expect_equal(length(unique(dc$colorscale[,2])), length(cats))

  # check error handling
  expect_error(discrete_coloring(seq.int(3), viridis::inferno(2)))
})




# ---------------------------------------------
# randorm data generation
test_that("generate demo data", {
  nr_stops <- 5
  nr_drives <- 10
  df <- generate_test_data(nr_stops, nr_drives)

  expect_equal(NROW(df), nr_stops * nr_drives)
})
