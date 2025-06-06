context("catmaply")


# -----------------------------------------------
# plot.R - error handling
# -----------------------------------------------
test_that("error handling - plot.R", {
  # get data
  df <- vbz[[1]] %>% dplyr::filter(.data$vehicle == "PO")
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
  # categorical_color_range not logical
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      categorical_color_range = 0,
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category"
    )
  )
  # categorical_color_range not logical
  expect_error(
    catmaply(
      df,
      x = trip_seq,
      y = stop_name,
      y_order = stop_seq,
      z = occupancy,
      categorical_color_range="BLA",
      categorical_col = occ_category,
      color_palette = viridis::inferno,
      slider=TRUE,
      rangeslider = FALSE,
      legend_interactive = FALSE
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
      slider=TRUE,
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
  # check xgap value
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      xgap = -1
    )
  )
  # check ygap value
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order = 'trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category",
      ygap = -1
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
      slider=TRUE,
      legend_interactive = FALSE,
      rangeslider = FALSE,
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
      categorical_color_range=TRUE,
      categorical_col = occ_category,
      color_palette = viridis::inferno,
      slider="BLA",
      rangeslider = FALSE,
      legend_interactive = FALSE
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
      slider=TRUE,
      legend_interactive = FALSE,
      rangeslider = FALSE,
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
  # check x_ordering and x matches
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      x_order='vehicle',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category"
    )
  )
  expect_error(
    catmaply(
      df,
      x='vehicle',
      x_order='trip_seq',
      y = "stop_name",
      y_order = "stop_seq",
      z = "occ_category"
    )
  )
  # check y_ordering and y matches
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      y = "stop_name",
      y_order='vehicle',
      z = "occ_category"
    )
  )
  # check y_ordering and y matches
  expect_error(
    catmaply(
      df,
      x='trip_seq',
      y = "vehicle",
      y_order='stop_name',
      z = "occ_category"
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
  # get data
  df <- vbz[[1]] %>% dplyr::filter(.data$vehicle == "PO")

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
      slider=TRUE,
      legend_interactive = FALSE,
      rangeslider = FALSE,
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
      slider=TRUE,
      legend_interactive = FALSE,
      rangeslider = FALSE,
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
      slider=TRUE,
      legend_interactive = FALSE,
      rangeslider = FALSE,
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
      slider=TRUE,
      legend_interactive = FALSE,
      rangeslider = FALSE,
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
      slider=TRUE,
      legend_interactive = FALSE,
      rangeslider = FALSE,
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
  # get data
  df <- vbz[[1]] %>% dplyr::filter(.data$vehicle == "PO")

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
    hover_hide = FALSE,
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
    hover_hide = TRUE,
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
    hover_hide = FALSE
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
    legend_interactive = FALSE
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
    legend_interactive = TRUE,
    legend = FALSE
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
    legend_interactive = FALSE,
    legend = FALSE
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
    rangeslider = TRUE
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
    rangeslider = FALSE
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
    legend_interactive = FALSE,
    hover_hide = TRUE
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
    legend = FALSE,
    hover_hide = TRUE
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
  fig <-
  expect_true(
    suppressWarnings(
      {
        is(
          catmaply(
          df,
          x = trip_seq,
          y = stop_name,
          y_order = stop_seq,
          z = occupancy,
          categorical_color_range=TRUE,
          categorical_col = occ_category,
          color_palette = viridis::inferno,
          slider=TRUE,
          rangeslider = FALSE,
          legend_interactive = FALSE
        ),
        "plotly"
        )
      }
    )
  )
})


# -----------------------------------------------
# catmaply time axis - plotting options
# -----------------------------------------------
test_that("test catmaply", {
  # get data
  df <- vbz[[1]] %>% dplyr::filter(.data$vehicle == "PO")
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
    legend_interactive = TRUE
  )
  expect_true(is(fig, "plotly"))

  # annotations and time axis
  fig <- catmaply(
    df,
    x=departure_date_time,
    y = "stop_name",
    y_order = "stop_seq",
    z = "occ_category",
    legend_interactive = FALSE,
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
    legend_interactive = TRUE,
    legend = FALSE
  )
  expect_true(is(fig, "plotly"))

  # time axis and no legend at all.
  fig <- catmaply(
    df,
    x=departure_date_time,
    y = "stop_name",
    y_order = "stop_seq",
    z = "occ_category",
    legend_interactive = FALSE,
    legend = FALSE
  )
  expect_true(is(fig, "plotly"))

  # time axis with date type input..
  dummy_df <- tibble(
    date = seq(as.Date("2017-01-01", tz='UTC'), as.Date("2017-02-01", tz='UTC'), by = 1),
    numeric_fill = sample(1000:2000, size = 32),
    categoric_fill = sample(1:5, size = 32, replace=T),
    numeric_x = sample(c(1:8), size = 32, replace = T),
    categoric_y = sample(LETTERS[1:5],size = 32, replace = T),

  )

  fig <- catmaply(
    dummy_df,
    x = date,
    y = categoric_y,
    z = categoric_fill,
    rangeslider = FALSE
  )
  expect_true(is(fig, "plotly"))

})


# -----------------------------------------------
# plot.R - warnings
# -----------------------------------------------
test_that("warnings- plot.R", {
  # get data
  df <- vbz[[1]] %>% dplyr::filter(.data$vehicle == "PO")
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
      slider_currentvalue_visible=FALSE,
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
      slider=TRUE,
      legend_interactive = TRUE,
      rangeslider = FALSE
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
      slider=TRUE,
      legend_interactive = FALSE,
      rangeslider = TRUE
    )
  )
})


# -----------------------------------------------
# plot.R - colorbar
# -----------------------------------------------
test_that("colorbar - plot.R", {
  # get data
  df <- vbz[[1]] %>% dplyr::filter(.data$vehicle == "PO")

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
      categorical_color_range = TRUE,
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
      categorical_color_range = TRUE,
      categorical_col = 'occ_category',
      color_palette = viridis::inferno(6),
      legend = TRUE
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
        categorical_color_range = TRUE,
        categorical_col = 'occ_category',
        color_palette = viridis::inferno
      ),
      "plotly"
    )
  )

  # color palette with static legend
  expect_true(
    suppressWarnings(
      {
        is(
          catmaply(
            df,
            x='trip_seq',
            x_order = 'trip_seq',
            x_tickangle = -10,
            y = "stop_name",
            y_order = "stop_seq",
            z = "occupancy",
            categorical_color_range = TRUE,
            categorical_col = 'occ_category',
            color_palette = viridis::inferno,
            legend_interactive = FALSE,
            legend = TRUE
          ),
          "plotly"
        )
      }
    )
  )

  # color palette with static legend and NAs in occ_category, occ_cat_name and occupancy
  df_na <- vbz[[1]] %>% dplyr::filter(.data$vehicle == "PO")%>%
    dplyr::mutate(
      occ_category = ifelse(occupancy<10, NA, occ_category),
      occ_cat_name= ifelse(occupancy<10, NA, occ_cat_name),
      occupancy = ifelse(occupancy<10, NA, occupancy))

  expect_true(
    suppressWarnings(
      {
        is(
          catmaply(
            df_na,
            x='trip_seq',
            x_order = 'trip_seq',
            x_tickangle = -10,
            y = "stop_name",
            y_order = "stop_seq",
            z = "occupancy",
            categorical_color_range = TRUE,
            categorical_col = 'occ_category',
            color_palette = viridis::inferno,
            legend_interactive = FALSE,
            legend = TRUE
          ),
          "plotly"
        )
      }
    )
  )



})
