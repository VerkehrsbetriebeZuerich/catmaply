% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layout.R
\name{catmaply_layout}
\alias{catmaply_layout}
\title{Adds time layout to catmaply object}
\usage{
catmaply_layout(
  fig,
  df,
  x,
  x_order,
  x_side,
  x_tickangle,
  x_range,
  y,
  y_order,
  y_side,
  y_tickangle,
  font_family,
  font_size,
  font_color,
  legend,
  rangeslider
)
}
\arguments{
\item{fig}{plotly object}

\item{df}{data.frame or tibble holding the data.}

\item{x}{column name holding the axis values for x.}

\item{x_order}{column name holding the ordering axis values for x. if no order is specified, then x will be used for ordering x; (default:"x").}

\item{x_side}{on which side the axis labels on the x axis should appear. options: c("top", "bottom"); (default:"top").}

\item{x_tickangle}{the angle of the axis label on the x axis. options: range -180 until 180; (default:90).}

\item{x_range}{the initial range that should be displayed on the x axis. Only works with non-time x-axis at the moment; (default: 30).}

\item{y}{column name holding the axis values for y.}

\item{y_order}{column name holding the ordering axis values for y. if no order is specified, then y will be used for ordering y; (default:"y").}

\item{y_side}{on which side the axis labels on the y axis should appear. options: c("left", "right"); (default:"left").}

\item{y_tickangle}{the angle of the axis label on the x axis. options: range -180 until 180; (default:0).}

\item{font_family}{the typeface that will be applied by the web browser.
The web browser will only be able to apply a font if it is available on the system which it operates.
Provide multiple font families, separated by commas, to indicate the preference in which to apply fonts if they aren't available on the system;
(default: c("Open Sans", "verdana", "arial", "sans-serif")).}

\item{font_size}{font size to be used for plot. needs to be a number greater than or equal to 1; (default: 12).}

\item{font_color}{font color to be used for plot; (default: "#444")}

\item{legend}{boolean indicating if legend should be displayed or not; (default: TRUE).}

\item{rangeslider}{whether or not the rangeslider should be displayed or not; (default: TRUE).}
}
\value{
plot_ly object
}
\description{
This layout is used if the x axis is of type time.
}
\keyword{internal}
