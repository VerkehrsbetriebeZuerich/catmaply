context("catmaply")

# get data
df <- vbz[[1]]

# -----------------------------------------------
# plot.R - error handling
# -----------------------------------------------
test_that("error handling - plot.R", {
  # wrong data type for df
  expect_error(
    catmaply(
      as.matrix(df),
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category"
    )
  )
  # categorical_colorbar not logical
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      categorical_colorbar = 0,
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category"
    )
  )
  # categorical_colorbar not logical
  expect_error(
    catmaply(
      df,
      x = trip_seq,
      y = stop_name,
      y_order = stop_seq,
      z = occupancy,
      categorical_colorbar="BLA",
      categorical_col = occ_category,
      color_palette = viridis::inferno,
      slider=T,
      rangeslider = F,
      legend_interactive = F
    )
  )
  # non-existant column name
  expect_error(
    catmaply(
      df,
      x='bla',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category"
    )
  )
  # y_side not valid
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      text=occ_category,
      slider=T,
      y_side="oben"
    )
  )
  # x_side not valid
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      x_side = 'the other one',
      z = "occ_category"
    )
  )
  # check tick angle range
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      x_tickangle = -9000,
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category"
    )
  )
  # worng font size
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      font_size = 0,
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category"
    )
  )
  # wrong text size
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      text_size = 0,
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category"
    )
  )
  # legend not logical
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      legend = ""
    )
  )
  # legend_interactive not logical
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      legend_interactive = ""
    )
  )
  # hover_hide not logical
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      hover_hide = ""
    )
  )
  # wrong parameter for rangeslider
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      rangeslider = "bla"
    )
  )
  # slider step is not list
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      text=occ_category,
      slider=T,
      legend_interactive = F,
      rangeslider = F,
      slider_steps=c(
        slider_start=1,
        slider_range=15,
        slider_shift=5,
        slider_step_name="occ_category"
      )
    )
  )
  # range not as number
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      x_range = "0",
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
    )
  )
  # slider not logical
  expect_error(
    catmaply(
      df,
      x = trip_seq,
      y = stop_name,
      y_order = stop_seq,
      z = occupancy,
      categorical_colorbar=T,
      categorical_col = occ_category,
      color_palette = viridis::inferno,
      slider="BLA",
      rangeslider = F,
      legend_interactive = F
    )
  )
  # slider step is not list
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      text=occ_category,
      slider=T,
      legend_interactive = F,
      rangeslider = F,
      slider_currentvalue_prefix = 1
    )
  )
  # check legend col matches category
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      legend_col = c(1,2,3,4)
    )
  )
  # check legend col matches category
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      legend_col = stop_seq
    )
  )
  # check legend col matches category
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      legend_col = "bla"
    )
  )
  # wrong color palette data type
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      color_palette = list(1, 2, 3)
    )
  )
  # wrong color palette
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      color_palette = c("#444", "#444", "#444")
    )
  )
  # hover_template references wrong column
  expect_error(
    catmaply(
      df,
      x = trip_seq,
      y = stop_name,
      y_order = stop_seq,
      z = occ_category,
      hover_template = paste(trip_seq, fs)
    )
  )
})

# -----------------------------------------------
# trace.R - error handling
# -----------------------------------------------
test_that("error handling - trace.R", {

  # ---------------------------------------------
  # not all steps defined
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      text=occ_category,
      slider=T,
      legend_interactive = F,
      rangeslider = F,
      slider_steps=list(
        slider_start=1,
        slider_range=15,
        slider_shift=5
      )
    )
  )
  # not valid column name for step_name_col
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      text=occ_category,
      slider=T,
      legend_interactive = F,
      rangeslider = F,
      slider_steps=list(
        slider_start=1,
        slider_range=15,
        slider_shift=5,
        slider_step_name="BLA"
      )
    )
  )
  # not valid column name for step_name_col
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      text=occ_category,
      slider=T,
      legend_interactive = F,
      rangeslider = F,
      slider_steps=list(
        slider_start=1,
        slider_range=15,
        slider_shift=5,
        slider_step_name="occ_category"
      )
    )
  )
  # no name for step defined
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      text=occ_category,
      slider=T,
      legend_interactive = F,
      rangeslider = F,
      slider_steps = list(
        list(range=c(1, 30)),
        list(name="nachmittag", range=c(31, 50))
      )
    )
  )
  # lower bound is higher than upper bound
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      text=occ_category,
      slider=T,
      legend_interactive = F,
      rangeslider = F,
      slider_steps = list(
        list(name="nachmittag", range=c(30, 1)),
        list(name="nachmittag", range=c(31, 50))
      )
    )
  )
})

# -----------------------------------------------
# catmaply - plotting options
# -----------------------------------------------
test_that("test catmaply", {

  # simple plot - colname with quotes
  fig <- catmaply(
    df,
    x='trip_seq',
    y = "stop_name",
    y_order = "stop_seq",
    z = "occ_category"
  )
  expect_true(is(fig, "plotly"))

  # simple plot - colname without quotes
  fig <- catmaply(
    df,
    x=trip_seq,
    x_order = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category
  )
  expect_true(is(fig, "plotly"))

  fig <- catmaply(
    df,
    x=trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category
  )
  expect_true(is(fig, "plotly"))

  # different legend_col
  fig <- catmaply(
    df,
    x=trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    legend_col = occ_category
  )
  expect_true(is(fig, "plotly"))

  # test hover_template
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    hover_hide = F,
    hover_template = paste(
      '<b>This</b>:', trip_seq,
      '<br><b>That</b>:', stop_seq,
      '<br><b>here</b>:', occ_category,
      '<extra></extra>'
    )
  )
  expect_true(is(fig, "plotly"))

  # test hover_template - but no hover :-)
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    hover_hide = T,
    hover_template = paste(
      '<b>This</b>:', trip_seq,
      '<br><b>That</b>:', stop_seq,
      '<br><b>here</b>:', occ_category,
      '<extra></extra>'
    )
  )
  expect_true(is(fig, "plotly"))

  # default hover
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    hover_hide = F
  )
  expect_true(is(fig, "plotly"))

  # simple hover template
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    hover_template = paste(stop_name)
  )
  expect_true(is(fig, "plotly"))

  # no interactive legend
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    hover_template = paste(stop_name),
    legend_interactive = F
  )
  expect_true(is(fig, "plotly"))

  # no legend but legend_interactive
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    hover_template = paste(stop_name),
    legend_interactive = T,
    legend = F
  )
  expect_true(is(fig, "plotly"))

  # no legend and no legend_interactive
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    hover_template = paste(stop_name),
    legend_interactive = F,
    legend = F
  )
  expect_true(is(fig, "plotly"))

  # rangeslider is true
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    hover_template = paste(stop_name),
    rangeslider = T
  )
  expect_true(is(fig, "plotly"))

  # no rangeslider
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    hover_template = paste(stop_name),
    rangeslider = F
  )
  expect_true(is(fig, "plotly"))

  # legend & no hover
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    hover_template = paste(stop_name),
    legend_interactive = F,
    hover_hide = T
  )
  expect_true(is(fig, "plotly"))

  # no legend & no hover
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    hover_template = paste(stop_name),
    legend = F,
    hover_hide = T
  )
  expect_true(is(fig, "plotly"))

  # very small range
  fig <- catmaply(
    df,
    x='trip_seq',
    x_order = 'trip_seq',
    x_range = 2,
    y = "stop_name",
    y_order = "stop_seq",
    z = "occ_category"
  )
  expect_true(is(fig, "plotly"))

  # too big color_palette
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occ_category,
    color_palette = viridis::inferno(10)
  )
  expect_true(is(fig, "plotly"))

  # other color palette
  fig <- catmaply(
    df,
    x = trip_seq,
    y = stop_name,
    y_order = stop_seq,
    z = occupancy,
    categorical_colorbar=T,
    categorical_col = occ_category,
    color_palette = viridis::inferno,
    slider=T,
    rangeslider = F,
    legend_interactive = F
  )
  expect_true(is(fig, "plotly"))
})


# -----------------------------------------------
# catmaply time axis - plotting options
# -----------------------------------------------
test_that("test catmaply", {
  # preprocess
  library(dplyr)
  # create departure_date_time
  df <- df %>%
    na.omit() %>%
    dplyr::group_by(
      trip_seq
    ) %>%
    dplyr::mutate(
      departure_date_time = min(na.omit(lubridate::ymd_hms(paste("2020-08-01", departure_time))))
    ) %>%
    dplyr::ungroup()

  # simple time plot
  fig <- catmaply(
    df,
    x=departure_date_time,
    y = "stop_name",
    y_order = "stop_seq",
    z = "occ_category",
    legend_interactive = T
  )
  expect_true(is(fig, "plotly"))

  # annotations and time axis
  fig <- catmaply(
    df,
    x=departure_date_time,
    y = "stop_name",
    y_order = "stop_seq",
    z = "occ_category",
    legend_interactive = F,
    text=occ_category
  )
  expect_true(is(fig, "plotly"))

  # time axis but no legend
  fig <- catmaply(
    df,
    x=departure_date_time,
    y = "stop_name",
    y_order = "stop_seq",
    z = "occ_category",
    legend_interactive = T,
    legend = F
  )
  expect_true(is(fig, "plotly"))

  # time axis and no legend at all.
  fig <- catmaply(
    df,
    x=departure_date_time,
    y = "stop_name",
    y_order = "stop_seq",
    z = "occ_category",
    legend_interactive = F,
    legend = F
  )
  expect_true(is(fig, "plotly"))
})


# -----------------------------------------------
# plot.R - warnings
# -----------------------------------------------
test_that("warnings- plot.R", {
  # too small range, warning
  expect_warning(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      x_range = 1,
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
    )
  )

  # currentvalue hidden but prefix
  expect_warning(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      text=occ_category,
      slider_currentvalue_visible=F,
      slider_currentvalue_prefix = "Warning"
    )
  )

  # rangeslider and interactive legend
  expect_warning(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      text=occ_category,
      slider=T,
      legend_interactive = T,
      rangeslider = F
    )
  )

  # rangeslider and slider
  expect_warning(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      text=occ_category,
      slider=T,
      legend_interactive = F,
      rangeslider = T
    )
  )
})


# -----------------------------------------------
# plot.R - colorbar
# -----------------------------------------------
test_that("colorbar - plot.R", {

  # too small color palette
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      x_tickangle = -10,
      y = "stop_name",
      y_order = "stop_seq",
      z = "occupancy",
      categorical_colorbar = T,
      categorical_col = 'occ_category',
      color_palette = viridis::inferno(6)
    )
  )
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      x_tickangle = -10,
      y = "stop_name",
      y_order = "stop_seq",
      z = "occupancy",
      categorical_colorbar = T,
      categorical_col = 'occ_category',
      color_palette = viridis::inferno(6),
      legend = T
    )
  )

  # dynamic color palette
  expect_true(
    is(
      catmaply(
        df,
        x='trip_seq',
        x_order = 'trip_seq',
        x_tickangle = -10,
        y = "stop_name",
        y_order = "stop_seq",
        z = "occupancy",
        categorical_colorbar = T,
        categorical_col = 'occ_category',
        color_palette = viridis::inferno
      ),
      "plotly"
    )
  )

  # color palette with static legend
  expect_true(
    is(
      catmaply(
        df,
        x='trip_seq',
        x_order = 'trip_seq',
        x_tickangle = -10,
        y = "stop_name",
        y_order = "stop_seq",
        z = "occupancy",
        categorical_colorbar = T,
        categorical_col = 'occ_category',
        color_palette = viridis::inferno,
        legend_interactive = F,
        legend = T
      ),
      "plotly"
    )
  )

  # no legend but categorical colun
  expect_warning(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      x_tickangle = -10,
      y = "stop_name",
      y_order = "stop_seq",
      z = "occupancy",
      categorical_colorbar = T,
      categorical_col = 'occ_category',
      color_palette = viridis::inferno,
      legend = F
    )
  )


})
